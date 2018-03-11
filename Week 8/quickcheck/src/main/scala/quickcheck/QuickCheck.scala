package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    i <- arbitrary[Int]
    heap <- oneOf(const(empty), genHeap)
  } yield insert(i, heap)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { h: H =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  /**
    * check that adding a single element to an empty heap, and then removing this element, should yield the element in question
    */
  property("min1") = forAll { i: A =>
    findMin(insert(i, empty)) == i
  }

  /**
    * If you insert any two elements into an empty heap, finding the minimum of the resulting
    * heap should get the smallest of the two elements back.
    */
  property("insertTwoElementsInEmptyHeap") = forAll { (i: A, a: A) =>
    val heap = insert(i, insert(a, empty))
    findMin(heap) == (if (a < i) a else i)
  }

  /**
    * If you insert an element into an empty heap, then delete the minimum, the resulting heap should be empty.
    */
  property("insertAndDeleteMinInHeap") = forAll { i: A =>
    deleteMin(insert(i, empty)) == empty
  }

  /**
    * Given any heap, you should get a sorted sequence of elements when continually finding and deleting minima.
    */
  property("sortHeap") = forAll { heap: H =>
    val h = sortRec(heap, Nil)
    h == h.sorted
  }

  /**
    * Finding a minimum of the melding of any two heaps should return a minimum of one or the other.
    */
  property("findMinOfMeldingTwoHeap") = forAll { (heap1: H, heap2: H) =>
    val meldHeap = meld(heap1, heap2)
    val minMeld = findMin(meldHeap)
    minMeld == findMin(heap1) || minMeld == findMin(heap2)
  }

  /**
    * Take two arbitrary heaps, meld together. Then remove min from 1 and insert into 2,
    * meld the results. Compare two melds by comparing sequences of ranks.
    */
  property("meldMin") = forAll { (heap1: H, heap2: H) =>
    val meld1 = meld(heap1, heap2)
    val min1 = findMin(heap1)
    val meld2 = meld(deleteMin(heap1), insert(min1, heap2))
    val sortedMeld1 = sortRec(meld1, Nil)
    val sortedMeld2 = sortRec(meld2, Nil)
    sortedMeld1 == sortedMeld2
  }

  /**
    * Find the minimal of the heap and inserted in an accumulated list of minimals and delete it from the heap to get
    * a sorted list of minimals
    * @param h the actual heap
    * @param acc the list of minimal int
    * @return list of sorted minimal
    */
  def sortRec(h: H, acc: List[Int]): List[Int] =
    if (isEmpty(h)) acc
    else findMin(h)::sortRec(deleteMin(h), acc)
}