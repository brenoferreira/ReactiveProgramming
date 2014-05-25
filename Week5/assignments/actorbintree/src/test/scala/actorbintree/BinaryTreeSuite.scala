/**
 * Copyright (C) 2009-2013 Typesafe Inc. <http://www.typesafe.com>
 */
package actorbintree

import akka.actor.{ Props, ActorRef, ActorSystem }
import org.scalatest.{ BeforeAndAfterAll, FlatSpec }
import akka.testkit.{ TestProbe, ImplicitSender, TestKit }
import org.scalatest.matchers.ShouldMatchers
import scala.util.Random
import scala.concurrent.duration._
import org.scalatest.FunSuite
import akka.actor.Actor

class FosterParent(childProps: Props, tester: ActorRef) extends Actor {
  val child = context.actorOf(childProps, "child")
  def receive = {
    case msg if context.sender == child => {
      tester forward msg
    }
    case msg => {
      child forward msg
    }
  }
}


class BinaryTreeSuite(_system: ActorSystem) extends TestKit(_system) with FunSuite with ShouldMatchers with BeforeAndAfterAll with ImplicitSender 
{

  def this() = this(ActorSystem("PostponeSpec"))

  override def afterAll: Unit = system.shutdown()

  import actorbintree.BinaryTreeSet._

  def receiveN(requester: TestProbe, ops: Seq[Operation], expectedReplies: Seq[OperationReply]): Unit =
    within(5.seconds) {
      val repliesUnsorted = for (i <- 1 to ops.size) yield try {
        requester.expectMsgType[OperationReply]
      } catch {
        case ex: Throwable if ops.size > 10 => fail(s"failure to receive confirmation $i/${ops.size}", ex)
        case ex: Throwable                  => fail(s"failure to receive confirmation $i/${ops.size}\nRequests:" + ops.mkString("\n    ", "\n     ", ""), ex)
      }
      val replies = repliesUnsorted.sortBy(_.id)
      if (replies != expectedReplies) {
        val pairs = (replies zip expectedReplies).zipWithIndex filter (x => x._1._1 != x._1._2)
        fail("unexpected replies:" + pairs.map(x => s"at index ${x._2}: got ${x._1._1}, expected ${x._1._2}").mkString("\n    ", "\n    ", ""))
      }
    }

  def verify(probe: TestProbe, ops: Seq[Operation], expected: Seq[OperationReply]): Unit = {
    val topNode = system.actorOf(Props[BinaryTreeSet])

    ops foreach { op =>
      topNode ! op
    }

    receiveN(probe, ops, expected)
  }

  test("proper inserts and lookups") {
    val topNode = system.actorOf(Props[BinaryTreeSet])

    topNode ! Contains(testActor, id = 1, 1)
    expectMsg(ContainsResult(1, false))

    topNode ! Insert(testActor, id = 2, 1)
    topNode ! Contains(testActor, id = 3, 1)

    expectMsg(OperationFinished(2))
    expectMsg(ContainsResult(3, true))
  }

  test("instruction example") {
    val requester = TestProbe()
    val requesterRef = requester.ref
    val ops = List(
      Insert(requesterRef, id=100, 1),
      Contains(requesterRef, id=50, 2),
      Remove(requesterRef, id=10, 1),
      Insert(requesterRef, id=20, 2),
      Contains(requesterRef, id=80, 1),
      Contains(requesterRef, id=70, 2)
      )
   
    val expectedReplies = List(
      OperationFinished(id=10),
      OperationFinished(id=20),
      ContainsResult(id=50, false),
      ContainsResult(id=70, true),
      ContainsResult(id=80, false),
      OperationFinished(id=100)     
      )

    verify(requester, ops, expectedReplies)
  }
  
  test("behave identically to built-in set (includes GC)") {
    val rnd = new Random()
    def randomOperations(requester: ActorRef, count: Int): Seq[Operation] = {
      def randomElement: Int = rnd.nextInt(100)
      def randomOperation(requester: ActorRef, id: Int): Operation = rnd.nextInt(4) match {
        case 0 => Insert(requester, id, randomElement)
        case 1 => Insert(requester, id, randomElement)
        case 2 => Contains(requester, id, randomElement)
        case 3 => Remove(requester, id, randomElement)
      }

      for (seq <- 0 until count) yield randomOperation(requester, seq)
    }

    def referenceReplies(operations: Seq[Operation]): Seq[OperationReply] = {
      var referenceSet = Set.empty[Int]
      def replyFor(op: Operation): OperationReply = op match {
        case Insert(_, seq, elem) =>
          referenceSet = referenceSet + elem
          OperationFinished(seq)
        case Remove(_, seq, elem) =>
          referenceSet = referenceSet - elem
          OperationFinished(seq)
        case Contains(_, seq, elem) =>
          ContainsResult(seq, referenceSet(elem))
      }

      for (op <- operations) yield replyFor(op)
    }

    val requester = TestProbe()
    val topNode = system.actorOf(Props[BinaryTreeSet])
    val count = 1000

    val ops = randomOperations(requester.ref, count)
    val expectedReplies = referenceReplies(ops)

    ops foreach { op =>
      topNode ! op
      if (rnd.nextDouble() < 0.1) topNode ! GC
    }
    receiveN(requester, ops, expectedReplies)
  }



  test("empty leaf node confirms copy without actually copying to new node") {
    val fosterNode = system.actorOf(Props(classOf[FosterParent], BinaryTreeNode.props(0, initiallyRemoved = true), testActor))

    val newNode = system.actorOf(BinaryTreeNode.props(1, initiallyRemoved = true))

    fosterNode ! BinaryTreeNode.CopyTo(newNode)
    expectMsg(BinaryTreeNode.CopyFinished)

    newNode ! Contains(testActor, id = 0, 0)
    expectMsg(ContainsResult(0, false))
  }

  test("active leaf node confirms copy while actually copying to new node") {
    val fosterNode = system.actorOf(Props(classOf[FosterParent], BinaryTreeNode.props(0, initiallyRemoved = false), testActor))

    val newNode = system.actorOf(BinaryTreeNode.props(1, initiallyRemoved = true))

    fosterNode ! BinaryTreeNode.CopyTo(newNode)
    expectMsg(BinaryTreeNode.CopyFinished)

    newNode ! Contains(testActor, id = 0, 0)
    expectMsg(ContainsResult(0, true))
  }

  test("empty node with two children confirms copy while actually copying to new node") {
    val fosterNode = system.actorOf(Props(classOf[FosterParent], BinaryTreeNode.props(0, initiallyRemoved = true), testActor))

    val newNode = system.actorOf(BinaryTreeNode.props(1, initiallyRemoved = true))

    fosterNode ! Insert(testActor, 1, 1)
    expectMsg(OperationFinished(1))
    fosterNode ! Contains(testActor, id = 1, 1)
    expectMsg(ContainsResult(1, true))

    fosterNode ! Insert(testActor, -1, -1)
    expectMsg(OperationFinished(-1))
    fosterNode ! Contains(testActor, id = -1, -1)
    expectMsg(ContainsResult(-1, true))

    fosterNode ! BinaryTreeNode.CopyTo(newNode)
    expectMsg(BinaryTreeNode.CopyFinished)

    newNode ! Contains(testActor, id = 0, 0)
    expectMsg(ContainsResult(0, false))

    newNode ! Contains(testActor, id = 1, 1)
    expectMsg(ContainsResult(1, true))

    newNode ! Contains(testActor, id = -1, -1)
    expectMsg(ContainsResult(-1, true))
  }

  test("active node with two children confirms copy while actually copying to new node") {
    val fosterNode = system.actorOf(Props(classOf[FosterParent], BinaryTreeNode.props(0, initiallyRemoved = false), testActor))

    val newNode = system.actorOf(BinaryTreeNode.props(1, initiallyRemoved = true))

    fosterNode ! Insert(testActor, 1, 1)
    expectMsg(OperationFinished(1))
    fosterNode ! Contains(testActor, id = 1, 1)
    expectMsg(ContainsResult(1, true))

    fosterNode ! Insert(testActor, -1, -1)
    expectMsg(OperationFinished(-1))
    fosterNode ! Contains(testActor, id = -1, -1)
    expectMsg(ContainsResult(-1, true))

    fosterNode ! BinaryTreeNode.CopyTo(newNode)
    expectMsg(BinaryTreeNode.CopyFinished)

    newNode ! Contains(testActor, id = 0, 0)
    expectMsg(ContainsResult(0, true))

    newNode ! Contains(testActor, id = 1, 1)
    expectMsg(ContainsResult(1, true))

    newNode ! Contains(testActor, id = -1, -1)
    expectMsg(ContainsResult(-1, true))
  }

  test("empty node with left child confirms copy while actually copying to new node") {
    val fosterNode = system.actorOf(Props(classOf[FosterParent], BinaryTreeNode.props(0, initiallyRemoved = true), testActor))

    val newNode = system.actorOf(BinaryTreeNode.props(1, initiallyRemoved = true))

    fosterNode ! Insert(testActor, -1, -1)
    expectMsg(OperationFinished(-1))
    fosterNode ! Contains(testActor, id = -1, -1)
    expectMsg(ContainsResult(-1, true))

    fosterNode ! BinaryTreeNode.CopyTo(newNode)
    expectMsg(BinaryTreeNode.CopyFinished)

    newNode ! Contains(testActor, id = 0, 0)
    expectMsg(ContainsResult(0, false))

    newNode ! Contains(testActor, id = -1, -1)
    expectMsg(ContainsResult(-1, true))
  }

  test("active node with left child confirms copy while actually copying to new node") {
    val fosterNode = system.actorOf(Props(classOf[FosterParent], BinaryTreeNode.props(0, initiallyRemoved = false), testActor))

    val newNode = system.actorOf(BinaryTreeNode.props(1, initiallyRemoved = true))

    fosterNode ! Insert(testActor, -1, -1)
    expectMsg(OperationFinished(-1))
    fosterNode ! Contains(testActor, id = -1, -1)
    expectMsg(ContainsResult(-1, true))

    fosterNode ! BinaryTreeNode.CopyTo(newNode)
    expectMsg(BinaryTreeNode.CopyFinished)

    newNode ! Contains(testActor, id = 0, 0)
    expectMsg(ContainsResult(0, true))

    newNode ! Contains(testActor, id = -1, -1)
    expectMsg(ContainsResult(-1, true))
  }

  test("empty node with right child confirms copy while actually copying to new node") {
    val fosterNode = system.actorOf(Props(classOf[FosterParent], BinaryTreeNode.props(0, initiallyRemoved = true), testActor))

    val newNode = system.actorOf(BinaryTreeNode.props(1, initiallyRemoved = true))

    fosterNode ! Insert(testActor, 1, 1)
    expectMsg(OperationFinished(1))
    fosterNode ! Contains(testActor, id = 1, 1)
    expectMsg(ContainsResult(1, true))

    fosterNode ! BinaryTreeNode.CopyTo(newNode)
    expectMsg(BinaryTreeNode.CopyFinished)

    newNode ! Contains(testActor, id = 0, 0)
    expectMsg(ContainsResult(0, false))

    newNode ! Contains(testActor, id = 1, 1)
    expectMsg(ContainsResult(1, true))
  }

  test("active node with right child confirms copy while actually copying to new node") {
    val fosterNode = system.actorOf(Props(classOf[FosterParent], BinaryTreeNode.props(0, initiallyRemoved = false), testActor))

    val newNode = system.actorOf(BinaryTreeNode.props(1, initiallyRemoved = true))

    fosterNode ! Insert(testActor, 1, 1)
    expectMsg(OperationFinished(1))
    fosterNode ! Contains(testActor, id = 1, 1)
    expectMsg(ContainsResult(1, true))

    fosterNode ! BinaryTreeNode.CopyTo(newNode)
    expectMsg(BinaryTreeNode.CopyFinished)

    newNode ! Contains(testActor, id = 0, 0)
    expectMsg(ContainsResult(0, true))

    newNode ! Contains(testActor, id = 1, 1)
    expectMsg(ContainsResult(1, true))
  }

}