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
}

