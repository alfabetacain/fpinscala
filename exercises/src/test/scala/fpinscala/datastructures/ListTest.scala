import collection.mutable.Stack
import org.scalatest._
import org.scalatest.Assertions._
import fpinscala.datastructures._

class ListTest extends FlatSpec with Matchers {

  "Method tail" should "return the list without the first element" in {
   val ls = List(1,2,3)
   val result = List.tail(ls)
   assert(result == List(2,3))
  }

  it should "return nill if given nill" in {
    val result = List.tail(Nil)
    assert(result == Nil)
  }

  "Function setHead" should "replace the head of a list" in {
    val ls = List(1,2,3)
    val result = List.setHead(ls,4)
    assert(result == List(4,2,3))
  }

  it should "create a singleton list if given nil list" in {
    val result = List.setHead(Nil,1)
    assert(result == List(1))
  }

  "Function drop" should "return a single element if dropping 3 out of 4" in {
    val result = List.drop(List(1,2,3,4),3)
    assert(result == List(4))
  }

  it should "return nil if dropping more elements that are in the list" in {
    val result = List.drop(List(1,2,3),4)
    assert(result == Nil)
  }

  "Function dropWhile" should "drop items while predicate holds" in {
    val result = List.dropWhile(List(1,2,3),(a: Int) => a < 3)
    assert(result == List(3))
  }

  "Function init" should "returns all but the last element in a list" in {
    val result = List.init(List(1,2,3,4))
    assert(result == List(1,2,3))
  }

  "Function length" should "return the length of a given list" in {
    val result = List.length(List(1,2,3))
    assert(result == 3)
  }

  "Function foldRightViaFoldLeft" should "do the same as foldRight" in {
    val ls = List(2,4,8)
    val realFoldRight = List.foldRight(ls,"")((elem,acc) => acc.toString() + elem.toString())
    val notReal = List.foldRightViaFoldLeft(ls,"")((elem,acc) => acc.toString() + elem.toString())
    assert(notReal == realFoldRight)
  }
    "Function foldLeftViaFoldRight" should "do the same as foldLeft" in {
      val ls = List(2,4,8)
      val realFoldLeft = List.foldLeft(ls,Nil: List[Int])((acc,elem) => Cons(elem,acc))
      val notReal = List.foldLeftViaFoldRight(ls,Nil: List[Int])((acc,elem) => Cons(elem,acc))
      assert(realFoldLeft == notReal)
  }

  "Function map" should "return a new list of mapped elements" in {
    val result = List.map(List(1,2,3))(_ + 3)
    assert(List(4,5,6) == result)
  }


  "Function append" should "append list b to list a" in {
    val a = List(1,2,3)
    val b = List(4,5,6)
    val result = List.append(a,b)
    assert(List(1,2,3,4,5,6) == result)
  }

  "Function flatten" should "flatten a list of lists to a list" in {
    val ls = List(List(1,2),List(3,4),List(5,6))
    val result = List.flatten(ls)
    assert(List(1,2,3,4,5,6) == result)
  }

  "Function add1" should "add 1 to every element in the list" in {
    val ls = List(1,1,1)
    val result = List.add1(ls)
    assert(List(2,2,2) == result)
  }

  "Function doubleListToString" should "make each double in a list to a string" in {
    val ls = List(1.0,2.0)
    val result = List.doubleListToString(ls)
    assert(List("1.0","2.0") == result)
  }

  "Function map" should "be able to map integers to doubles" in {
    val ls = List(1,2)
    val result = List.map(ls)(_.toDouble)
    assert(List(1.0,2.0) == result)
  }

  "Function filter" should "be able to remove all odd integers from list" in {
    val ls = List(1,2,3,4,5,6)
    val result = List.filter(ls)(_ % 2 == 0)
    assert(List(2,4,6) == result)
  }

  "Function flatMap" should "pass book's test" in {
    val bookResult = List(1,1,2,2,3,3)
    val actual = List.flatMap(List(1,2,3))(i => List(i,i))
    assert(bookResult == actual)
  }

  "Function filterByFlatMap" should "be able to remove all odd integers" in {
    val ls = List(1,2,3,4,5,6)
    val result = List.filterByFlatMap(ls)(_ % 2 == 0)
    assert(List(2,4,6) == result)
  }

  "Function addLists" should "be able to add elements of two integer lists" in {
    val a1 = List(1,2,3)
    val a2 = List(4,5,6)
    val result = List.addLists(a1,a2)
    assert(List(5,7,9) == result)
  }

  "Function zipWith" should "be able to make two lists into one list of tuples" in {
    val a1 = List(1,2,3)
    val a2 = List("A","B","C")
    val result = List.zipWith(a1,a2)
    assert(List((1,"A"),(2,"B"),(3,"C")) == result)
  }

  "Function hasSubSequence" should "work with prefix being subsequence" in {
    val sup = List(1,2,3)
    val sub = List(1,2)
    assert(true == List.hasSubSequence(sup,sub))
  }

  it should "work with affix being subsequence" in {
    val sup = List(1,2,3)
    val sub = List(2,3)
    assert(true == List.hasSubSequence(sup,sub))
  }

  it should "work with middle being subsequence" in {
    val sup = List(1,2,3,4,5)
    val sub = List(2,3,4)
    assert(true == List.hasSubSequence(sup,sub))
  }

  it should "work with no subsequence" in {
    val sup = List(1,2,3,4,5)
    val sub = List(6,7)
    assert(false == List.hasSubSequence(sup,sub))
  }
}
