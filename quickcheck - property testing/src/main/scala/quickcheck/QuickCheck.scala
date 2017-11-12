package quickcheck

import common._
import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import com.sun.org.apache.xml.internal.security.algorithms.implementations.IntegrityHmac.IntegrityHmacMD5

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
   const(empty),
    for {
      i <- arbitrary[Int]
      h <- oneOf(const(empty), genHeap)
    } yield insert(i,h)
  )

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  //это данный нам тест,
  // из него видно, что в коллекцию можно вставить эл-т равный существующему
  //Но, возможно, возвращается просто последний вставленный элемента
  //возможно, при вставке эл-та равного сущ-му - он ничего не вставляет
  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

//  property("this is not s stack") = forAll { (h: H) =>
//    if (isEmpty(h)) true
//    else {
//      val m = findMin(h)
//      if (m==Int.MaxValue) true
//      else findMin(insert(m + 1, h)) == m
//    }
//  }
  property("this is not s stack") = forAll { (h: H) =>
    (!isEmpty(h)) ==>
    {
      val m = findMin(h)
      if (m==Int.MaxValue) true
      else findMin(insert(m + 1, h)) == m
    }
  }

  property("insert less value - obtains it") = forAll { (h: H) =>
    (!isEmpty(h)) ==>
    {
      val m = findMin(h)
      if (m==Int.MinValue) true
      else findMin(insert(m - 1, h)) == m - 1
    }
  }

  property("check insert on empty list") = forAll { (a: Int) =>
    findMin(insert(a, empty)) == a
  }

  property("check double insert on empty list") = forAll { (a: Int) =>
    (!a==Int.MaxValue) ==>
    {
    var h=insert(a, empty)
    h=insert(a+1, h)
    h=insert(a+2, h)
    findMin(h) == a
    }
  }

  property("check delete") = forAll { a: Int =>
    var h=insert(a, empty)
    h= deleteMin(h)
    isEmpty(h)
  }

  property("can insert and delete copies (is heap, not a set)") = forAll { (a: Int) =>
    var h=insert(a,empty)
    h=insert(a,h)
    h=insert(a,h)
    h=deleteMin(h)
    h=deleteMin(h)
    findMin(h)==a
  }

  property("can duplicate non-min values") = forAll { (a: Int) =>
    if (a > Int.MaxValue-2) true
    else
    {
      var h=insert(a,empty)
      h=insert(a+1,h)
      h=insert(a+1,h)
      h=insert(a+2,h)
      h=deleteMin(h)
      h=deleteMin(h)
      h=deleteMin(h)
      findMin(h)==a+2
    }
  }


  def checkThatMinIsHigher(h:H,previousMin:Int):Boolean=
    if (isEmpty(h)) true
    else {
      val curMin=findMin(h)
      if (curMin>=previousMin) checkThatMinIsHigher(deleteMin(h),curMin)
      else false
    }

  property("next minimal value is determined correctly") = forAll { (h: H) =>
    checkThatMinIsHigher(h,Int.MinValue)
  }

  property("melding with smaller on the left") = forAll { (h: H) =>
    if (isEmpty(h)) findMin(meld(insert(2,h),insert(1,h)))==1
    else {
    val m= findMin(h)
    if (m==Int.MinValue) true
    else {
    val h_smaller=insert(m-1,h)
    findMin(meld(h,h_smaller))==m-1}
    }
  }

  property("melding with smaller on the right") = forAll { (h: H) =>
    if (isEmpty(h)) findMin(meld(insert(1,h),insert(2,h)))==1
    else {
      val m = findMin(h)
      findMin(meld(h, deleteMin(h))) == m
    }
  }

  property("melding and check priority-queue property holds") = forAll { (h: H) =>

      forAll { (h1:H) =>
    if (isEmpty(h))
      findMin(meld(insert(1,h),insert(2,h)))==1
    else {
      val m = findMin(h)
      checkThatMinIsHigher(deleteMin(meld(h,h)),m)
    }
    }
  }

  //проверим генерацию и объединение со случайным сэмплом
  property("melding 2 random heaps and check priority-queue property holds") = forAll { (h: H,h2:H) =>
      checkThatMinIsHigher(meld(h,h2),Int.MinValue)
    }

  def checkSubMeldIsHigherAndComplete(h:H,subh1:H,subh2:H,previousMin:Int):Boolean=
    if (isEmpty(h)) {isEmpty(subh1)&isEmpty(subh2)}
    else {
      val curMin = findMin(h)
      if (curMin < previousMin) false
      else {

        if (!isEmpty(subh1)) {
          if (findMin(subh1) == curMin)
          checkSubMeldIsHigherAndComplete(deleteMin(h), deleteMin(subh1),subh2,curMin)
          else
            checkSubMeldIsHigherAndComplete(deleteMin(h), subh1,deleteMin(subh2),curMin)}
        else
          checkSubMeldIsHigherAndComplete(deleteMin(h), subh1,deleteMin(subh2),curMin)
      }
    }

  property("melding and check that all initial heap's elems were present") = forAll { (h1: H, h2:H) =>
      checkSubMeldIsHigherAndComplete(meld(h1,h2),
        h1,
        h2,
        Int.MinValue)
  }

  //после удаления оставшийся элемент - минимальный
  property("deletion is correct") = forAll { (a: Int) =>
    if (a > Int.MaxValue-2) true
    else
    {
      var h=insert(a,empty)
      h=insert(a+1,h)
      h=insert(a+1,h)
      h=insert(a+2,h)
      h=deleteMin(h)
      h=deleteMin(h)
      h=deleteMin(h)
      findMin(h)==a+2
    }
  }

}
