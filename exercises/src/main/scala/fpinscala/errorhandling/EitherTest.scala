import org.scalatest._
import org.scalatest.Assertions._
import fpinscala.errorhandling._

class EitherTest extends FlatSpec {

  "Function map" should "only run mapper if either is right" in {
    val e = Right("Hello ")
    val actual = e.map(x => x + x)
    assert(Right("Hello Hello ") == actual)
  }
  
  it should "not run if either is left" in {
    val e = Left("Hello"): Either[String,String]
    val actual = e.map(x => x + x)
    assert(Left("Hello") == actual)
  }

  "Function flatMap" should "not evaluate if value is Left" in {
    val e = Left("Hello")
    val actual = e.flatMap(x => Right("World"))
    assert(e == actual)
  }

  it should "evaluate if value is Right" in {
    val e = Right("Hello")
    val actual = e.flatMap(x => Right("World"))
    assert(Right("World") == actual)
  }

  "Function orElse" should "return optional value if this is Left" in {
    val e = Left("Hello")
    val actual = e.orElse(Right("World"))
    assert(Right("World") == actual)
  }

  it should "return actual value if this is Right" in {
    val e = Right("Hello")
    val actual = e.orElse(Left("World"))
    assert(e == actual)
  }

  "Function map2" should "run if both inputs are Right" in {
    val x = Right("Hello ")
    val y = Right("World")
    val actual = x.map2(y)((a,b) => a + b)
    assert(Right("Hello World") == actual)
  }

  "Function traverse" should "execute the function given as long as it does not return left" in {
    val l = List(1,2,3)
    val actual = Either.traverse(l)(x => Right(x+2))
    assert(Right(List(3,4,5)) == actual)
  }

  "Function sequence" should "retrieve the values of a list of rights" in {
    val l = List(Right(1),Right(2))
    val actual = Either.sequence(l)
    assert(Right(List(1,2)) == actual)
  }
}

