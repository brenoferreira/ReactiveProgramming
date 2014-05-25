package kvstore

import akka.actor._
import kvstore.Arbiter._
import scala.collection.immutable.Queue
import akka.actor.SupervisorStrategy.Restart
import scala.annotation.tailrec
import akka.pattern.{AskTimeoutException, ask, pipe}
import scala.concurrent.duration._
import akka.util.Timeout
import scala.concurrent.Future
import scala.util.{Failure, Success}
import scala.Some
import akka.actor.OneForOneStrategy
import scala.util.Success
import kvstore.Arbiter.Replicas

object Replica {
  sealed trait Operation {
    def key: String
    def id: Long
  }
  case class Insert(key: String, value: String, id: Long) extends Operation
  case class Remove(key: String, id: Long) extends Operation
  case class Get(key: String, id: Long) extends Operation

  sealed trait OperationReply
  case class OperationAck(id: Long) extends OperationReply
  case class OperationFailed(id: Long) extends OperationReply
  case class GetResult(key: String, valueOption: Option[String], id: Long) extends OperationReply

  def props(arbiter: ActorRef, persistenceProps: Props): Props = Props(new Replica(arbiter, persistenceProps))
}

class Replica(val arbiter: ActorRef, persistenceProps: Props) extends Actor {
  import Replica._
  import Replicator._
  import Persistence._
  import context.dispatcher
  import scala.language.postfixOps

  /*
   * The contents of this actor is just a suggestion, you can implement it in any way you like.
   */
  
  var kv = Map.empty[String, String]
  // a map from secondary replicas to replicators
  var secondaries = Map.empty[ActorRef, ActorRef]
  // the current set of replicators
  var replicators = Set.empty[ActorRef]

  implicit val timeout = Timeout(1.second)

  val persister = context.actorOf(persistenceProps)

  override val supervisorStrategy = OneForOneStrategy() {
    case _:PersistenceException => Restart
  }

  arbiter ! Join

  def receive = {
    case JoinedPrimary   => context.become(leader)
    case JoinedSecondary => context.become(replica)
  }

  /* TODO Behavior for  the leader role. */
  val leader: Receive = {

    case value:Insert => insert(value)
    case value:Remove => remove(value)
    case value:Get => get(value)
    case Replicas(replicas) => {
      initialReplication(replicas)
    }
    case x:Timeout => sender ! OperationFailed(0L)
  }

  def initialReplication(replicas: Set[ActorRef]) {
    secondaries.filter({
      case (replica, replicator) => !replicas.contains(replica)
    }).foreach {
      case (_, replicator) => context stop replicator
    }

    secondaries = replicas.drop(1)
      .map(replica => (replica, context.actorOf(Props(classOf[Replicator], replica))))
      .toMap

    replicators = secondaries.values.toSet

    for ((key, value) <- kv; (replica, replicator) <- secondaries) {
      //replica ! Snapshot(key, Some(value), lastSeq+1)
      replicator ! Replicate(key, Some(value), lastSeq + 1)
      lastSeq = nextSeq
    }
  }

  def insert(value:Insert) = {
    kv = kv + ((value.key, value.value))
    //sender ! OperationAck(value.id)

    replicate(value, sender)
  }

  def remove(value:Remove) = {
    kv = kv - value.key

    //sender ! OperationAck(value.id)

    replicate(value, sender)
  }

  def replicate(op:Operation, senderActor:ActorRef) = {
    val replicateMsg = op match {
      case Insert(key, value, id) => Replicate(key, Some(value), id)
      case Remove(key, id) => Replicate(key, None, id)
    }

    var failed = false

    context.system.scheduler.scheduleOnce(1.2.second) {
      if(failed) senderActor ! OperationFailed(op.id)
    }

    val replications = replicators
      .map(replicator =>(replicator ? replicateMsg).mapTo[Replicated])

    val sequence = Future.sequence(replications)
    sequence
      .onSuccess {
        case replicated => persist(senderActor,
          OperationAck(op.id),
          Persist(replicateMsg.key,
            replicateMsg.valueOption,
            replicateMsg.id))
      }

    sequence.onFailure { case _ => failed = true }
  }

  def get(value:Get) = {
    kv get value.key match {
      case x => sender ! GetResult(value.key, x, value.id)
    }
  }

  def persist(senderActor:ActorRef, reply:AnyRef, persistMsg:Persist):Unit = {
    var counter = 0
    context.system.scheduler.schedule(0 millis, 100 millis) {
      counter += 1

      if(counter < 10)
        ask(persister, persistMsg)
          .mapTo[Persisted]
          .map {
            case _ => counter = 11; senderActor ! reply
          }
      else if(counter == 10) senderActor ! OperationFailed(persistMsg.id)
      else ()
    }

  }

  var lastSeq:Long = -1
  def nextSeq = lastSeq + 1

  /* TODO Behavior for the replica role. */
  val replica: Receive = {
    case value:Get => get(value)
    case Snapshot(key, Some(value), seq) if seq == nextSeq => {
      lastSeq = seq
      kv = kv + ((key, value))

      persist(sender, SnapshotAck(key, seq), Persist(key, Some(value), seq))
    }
    case Snapshot(key, None, seq) if seq == nextSeq => {
      lastSeq = seq
      kv = kv - (key)

      persist(sender, SnapshotAck(key, seq), Persist(key, None, seq))
    }
    case Snapshot(key, _, seq) if seq <= lastSeq => sender ! SnapshotAck(key, seq)
    case Snapshot(key, _, seq) if seq > nextSeq =>
  }

}
