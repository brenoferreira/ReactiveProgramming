package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("insert 2 to empty heap, min should equal min of a & b") = forAll { (a:Int, b:Int) =>
    val h1 = insert(a, empty)
    val h2 = insert(b, h1)

    findMin(h2) == Math.min(a, b)
  }

  property("insert to empty heap, deleteMin, than should be empty") = forAll { (a:Int, b:Int) =>
    val h1 = insert(a, empty)
    val h2 = insert(b, h1)

    val emptyHeap = deleteMin(deleteMin(h2))
    isEmpty(emptyHeap)
  }

  property("findMin of meld should be min of one or the other") = forAll { (a:H, b:H) =>
    val min1 = findMin(a)
    val min2 = findMin(b)
    val min3 = findMin(meld(a, b))

    min3 == min1 || min3 == min2
  }

  property("findMin and deleteMin should result in ordered set") = forAll{ (a:H) =>
    def map(h:H):List[A] = {
      if(isEmpty(h)) Nil
      else findMin(h) :: map(deleteMin(h))
    }

    val list = map(a)

    list.sorted == list
  }

  property("bogus 4") = forAll { (a:H, b:H) => 
    def map(h:H):List[A] = {
      if(isEmpty(h)) Nil
      else findMin(h) :: map(deleteMin(h))
    }

    val melded1 = meld(a, b)

    val min1 = findMin(a)
    val oneWithoutMin1 = deleteMin(a)
    val twoWithMin1 = insert(min1, b)

    val melded2 = meld(oneWithoutMin1, twoWithMin1)

    val list1 = map(melded1)
    val list2 = map(melded2)

    list1 == list2
  }

  lazy val genHeap: Gen[H] = for {
    x <- arbitrary[Int]
    h <- Gen.frequency((1, value(empty)), (10, genHeap))
  } yield insert(x, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
