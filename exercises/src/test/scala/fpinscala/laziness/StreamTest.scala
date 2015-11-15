import org.scalatest._
import org.scalatest.Assertions._
import fpinscala.laziness._

class StreamTest extends FlatSpec {

  "toList" should "should turn a stream into a list, thus evaluating everything" in {
    val s = Stream(1,2,3)
    val actual = s.toList
    assert(List(1,2,3) == actual)
  }

  "take" should "return the first n elements of a stream" in {
    val s = Stream(1,2,3,4)
    val actual = s.take(3).toList
    assert(List(1,2,3) == actual)
  }

  it should "return empty if no elements are present" in {
    val s = Stream.empty
    val actual = s.take(2)
    assert(Stream.empty == actual)
  }
  
  it should "return as many elements as possible if n is higher than the stream length" in {
    val s = Stream(1,2)
    val actual = s.take(3).toList
    assert(List(1,2) == actual)
  }

  "drop" should "drop the first n elements of a stream" in {
    val s = Stream(1,2,3)
    val actual = s.drop(2).toList
    assert(List(3) == actual)
  }

  it should "return empty when there are no elements to drop" in {
    val s = Stream.empty
    val actual = s.drop(2)
    assert(Stream.empty == actual)
  }

  it should "drop as many elements as possible if n is higher than the stream length" in {
    val s = Stream(1,2)
    val actual = s.drop(4)
    assert(Stream.empty == actual)
  }

  "takeWhile" should "return the prefix of a stream that satisfies a given predicate" in {
    val s = Stream(1,2,3)
    val actual = s.takeWhile(_ < 3).toList
    assert(List(1,2) == actual)
  }

  it should "return everything if everything matches predicate" in {
    val s = Stream(1,2,3)
    val actual = s.takeWhile(_ < 5).toList
    assert(List(1,2,3) == actual)
  }

  it should "return empty if the first element does not satisfy predicate" in {
    val s = Stream(1,2,3)
    val actual = s.takeWhile(_ < 0)
    assert(Stream.empty == actual)
  }

  it should "return empty if the stream is empty" in {
    val s = Stream.empty: Stream[Int]
    val actual = s.takeWhile(_ < 3)
    assert(Stream.empty == actual)
  }

  "forAll" should "check if all elements satisfy predicate" in {
    val s = Stream(1,2,3,4)
    val actual = s.forAll(_ < 5)
    assert(true == actual)
  }

  "headOption" should "return the first element of a nonempty stream" in {
    val s = Stream(1,2)
    val actual = s.headOption
    assert(Some(1) == actual)
  }

  it should "return none if stream is empty" in {
    val s = Stream.empty
    val actual = s.headOption
    assert(None == actual)
  }

  "map" should "map each element in a stream to something" in {
    val s = Stream(1,2,3)
    val actual = s.map(_ + 2).toList
    assert(List(3,4,5) == actual)
  }

  it should "return empty when used on an empty stream" in {
    val actual = (Stream.empty: Stream[Int]).map(_ + 2)
    assert(Stream.empty == actual)
  }

  "filter" should "return the elements satisfying the given predicate" in {
    val s = Stream(1,2,3)
    val actual = s.filter(_ % 2 != 0).toList
    assert(List(1,3) == actual)
  }

  it should "return empty when used on an empty stream" in {
   val actual = (Stream.empty: Stream[Int]).filter(_ % 2 == 0)
   assert(Stream.empty == actual)
  }

  "append" should "append the second argument to the first" in {
    val s1 = Stream(1,2,3)
    val s2 = Stream(4,5,6)
    val actual = s1.append(s2).toList
    assert(List(1,2,3,4,5,6) == actual)
  }

  it should "return empty when appending empty to empty" in {
    val actual = Stream.empty.append(Stream.empty)
    assert(Stream.empty == actual)
  }

  "flatMap" should "work" in {
    val s = Stream(1,2)
    val actual = s.flatMap(x => Stream(x,x)).toList
    assert(List(1,1,2,2) == actual)
  }

  "constant" should "return a stream of constants" in {
    val s = Stream.constant("Hello")
    val actual = s.take(2).toList
    assert(List("Hello","Hello") == actual)
  }

  "from" should "return a stream of increasing integers" in {
    val s = Stream.from(1)
    val actual = s.take(3).toList
    assert(List(1,2,3) == actual)
  }

  "fibs" should "return the fibonacci sequence" in {
    val s = Stream.fibs(0,1)
    val actual = s.take(3).toList
    assert(List(0,1,1) == actual)
  }

  "startsWith" should "return true if first starts with second" in {
    val first = Stream(1,2,3)
    val second = Stream(1,2)
    assert(true == first.startsWith(second))
  }
}
