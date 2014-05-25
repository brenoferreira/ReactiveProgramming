/**
 * Copyright (C) 2009-2013 Typesafe Inc. <http://www.typesafe.com>
 */
package actorbintree

import akka.actor._
import scala.collection.immutable.Queue

object BinaryTreeSet {

  trait Operation {
    def requester: ActorRef
    def id: Int
    def elem: Int
  }

  trait OperationReply {
    def id: Int
  }

  /** Request with identifier `id` to insert an element `elem` into the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Insert(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to check whether an element `elem` is present
    * in the tree. The actor at reference `requester` should be notified when
    * this operation is completed.
    */
  case class Contains(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to remove the element `elem` from the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Remove(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request to perform garbage collection*/
  case object GC

  /** Holds the answer to the Contains request with identifier `id`.
    * `result` is true if and only if the element is present in the tree.
    */
  case class ContainsResult(id: Int, result: Boolean) extends OperationReply
  
  /** Message to signal successful completion of an insert or remove operation. */
  case class OperationFinished(id: Int) extends OperationReply

}


class BinaryTreeSet extends Actor {
  import BinaryTreeSet._
  import BinaryTreeNode._

  def createRoot: ActorRef = context.actorOf(BinaryTreeNode.props(0, initiallyRemoved = true))

  var root = createRoot

  // optional
  var pendingQueue = Queue.empty[Operation]

  // optional
  def receive = normal

  // optional
  /** Accepts `Operation` and `GC` messages. */
  val normal: Receive = {
//    case Contains(requester, id, elem) => root ! Contains(requester, id, elem)
//    case Insert(requester, id, elem) => root ! Insert(requester, id, elem)
//    case Remove(requester, id, elem) => root ! Remove(requester, id, elem)
    case GC => {
      val newRoot = createRoot
      root ! CopyTo(newRoot)

      context.become(garbageCollecting(newRoot))
    }
    case message:Operation => root ! message
  }

  // optional
  /** Handles messages while garbage collection is performed.
    * `newRoot` is the root of the new binary tree where we want to copy
    * all non-removed elements into.
    */
  def garbageCollecting(newRoot: ActorRef): Receive = {
    case CopyFinished =>
      context.stop(root)
      root = newRoot
      sender ! CopyFinished

      while(!pendingQueue.isEmpty) {
        val dequeue = pendingQueue.dequeue
        root ! dequeue._1
        pendingQueue = dequeue._2
      }

      context.become(normal)
    case x:Operation => pendingQueue = pendingQueue enqueue x
  }

}

object BinaryTreeNode {
  trait Position

  case object Left extends Position
  case object Right extends Position

  case class CopyTo(treeNode: ActorRef)
  case object CopyFinished

  def props(elem: Int, initiallyRemoved: Boolean) = Props(classOf[BinaryTreeNode],  elem, initiallyRemoved)
}

class BinaryTreeNode(val elem: Int, initiallyRemoved: Boolean) extends Actor {
  import BinaryTreeNode._
  import BinaryTreeSet._

  var subtrees = Map[Position, ActorRef]()
  var removed = initiallyRemoved

  // optional
  def receive = normal

  def contains(requester:ActorRef, id:Int, el:Int) = {
    val pos = if(el < elem) Left else Right
    if(elem == el) requester ! ContainsResult(id, !removed)
    else if(!subtrees.isEmpty && subtrees.contains(pos)) subtrees(pos) ! Contains(requester, id, el)
    else requester ! ContainsResult(id, false)
  }

  def insert(requester:ActorRef, id:Int, el:Int) = {
    val pos = if(el < elem) Left else Right
    if(elem == el) {
      removed = false
      requester ! OperationFinished(id)
    }
    else if(subtrees.contains(pos))
      subtrees(pos) ! Insert(requester, id, el)
    else {
      subtrees = subtrees + ((pos, context.actorOf(props(el, false))))
      requester ! OperationFinished(id)
    }
  }

  def remove(requester:ActorRef, id:Int, el:Int) = {
    val pos = if(el < elem) Left else Right
    if(elem == el) {
      removed = true
      requester ! OperationFinished(id)
    }
    else if(subtrees.contains(pos))
      subtrees(pos) ! Remove(requester, id, el)
    else requester ! OperationFinished(id)
  }

  // optional
  /** Handles `Operation` messages and `CopyTo` requests. */
  val normal: Receive = {
    case Contains(requester, id, el) => contains(requester, id, el)
    case Insert(requester, id, el) => insert(requester, id, el)
    case Remove(requester, id, el) => remove(requester, id, el)
    case CopyTo(treeNode) => {
      val children = subtrees.values.toSet

      removed match {
        case true if children.isEmpty => context.parent ! CopyFinished
        case _ => {
          if (!removed) treeNode ! Insert(self, 100, elem)
          children map { _ ! CopyTo(treeNode) }
          context become copying(children, removed)
        }
      }
    }
  }

  // optional
  /** `expected` is the set of ActorRefs whose replies we are waiting for,
    * `insertConfirmed` tracks whether the copy of this node to the new tree has been confirmed.
    */
  def copying(expected: Set[ActorRef], insertConfirmed: Boolean): Receive = {
    case CopyFinished if (insertConfirmed && expected.size == 1) => context.parent ! CopyFinished
    case OperationFinished(100) if (expected.isEmpty) => context.parent ! CopyFinished

    case CopyFinished => context become copying(expected - sender, insertConfirmed)
    case OperationFinished(_) => context.become(copying(expected, true))
  }

}
