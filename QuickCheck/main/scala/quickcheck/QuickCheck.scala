package quickcheck

import org.scalacheck.*
import Arbitrary.*
import Gen.*
import Prop.forAll

import scala.annotation.tailrec
import scala.collection.Iterable.single

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap:
  lazy val genHeap: Gen[H] = {
    for {
      x <- arbitrary[A]
      h <- oneOf(const(empty), genHeap)
    } yield insert(x, h)
  }

  given Arbitrary[H] = Arbitrary(genHeap)

  private def getHeapElements(h: H): List[A] = {
    if (isEmpty(h)) {
      Nil
    } else {
      val x = findMin(h)
      x :: getHeapElements(deleteMin(h))
    }
  }

  private def heapFromList(xs: List[A], h: H): H = {
    xs match {
      case Nil => h
      case y :: ys => heapFromList(ys, insert(y, h))
    }
  }

  // Inserting and removing the smallest element should give that element.
  property("gen1") = forAll { (h: H) =>
    val m = if isEmpty(h) then 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  // Inserting 2 elements and calling findMin gives the smallest
  property("insert2ElementsInEmpty") = forAll { (a1: A, a2: A) =>
    val h1 = insert(a1, empty)
    val h2 = insert(a2, h1)
    findMin(h2) == (if a1 < a2 then a1 else a2)
  }

  // Deleting from a heap with 1 element results in an empty heap.
  property("deletingFrom1ElementHeapGivesEntity") = forAll { (a: A) =>
    val h1 = insert(a, empty)
    val h2 = deleteMin(h1)
    isEmpty(h2)
  }

  // Removing the elements from a heap one by one gives an ordered
  // traversal of the heap.
  // To get this we generate an arbitrary list and insert all elements into the
  // heap, then assert that removing all these elements gives the sorted list.
  property("removesInOrder") = forAll { (l: List[A]) =>
    val h = heapFromList(l, empty)
    getHeapElements(h) == l.sorted
  }

  // The min of melding 2 heaps is the minimum of the min of each heap.
  property("minOfMeldingTwoHeaps") = forAll { (h1: H, h2: H) =>
    val m1 = findMin(h1)
    val m2 = findMin(h2)
    findMin(meld(h1, h2)) == (if m1 < m2 then m1 else m2)
  }

