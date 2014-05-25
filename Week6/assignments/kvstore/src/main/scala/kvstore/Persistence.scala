package kvstore

import akka.actor.{OneForOneStrategy, Props, Actor}
import scala.util.Random
import java.util.concurrent.atomic.AtomicInteger
import kvstore.Persistence.{Persist, PersistenceException}
import akka.actor.SupervisorStrategy.Restart
import akka.util.Timeout
import scala.concurrent.duration._

object Persistence {
  case class Persist(key: String, valueOption: Option[String], id: Long)
  case class Persisted(key: String, id: Long)

  class PersistenceException extends Exception("Persistence failure")

  def props(flaky: Boolean): Props = Props(classOf[Persistence], flaky)
}


class Persistence(flaky: Boolean) extends Actor {
  import Persistence._

  def receive = {
    case Persist(key, _, id) =>
      if (!flaky || Random.nextBoolean()) sender ! Persisted(key, id)
      else throw new PersistenceException
  }

}
