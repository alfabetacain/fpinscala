import org.scalatest._
import org.scalatest.Assertions._
import fpinscala.errorhandling._

class OptionTest extends FlatSpec {

  "Function map" should "map a some value to another some value" in {
    val op = Some(2)
    val result = op.map(value => value.toString())
    assert(Some("2") == result)
  }

  it should "map a none to a none" in {
    val result = None.map(value => value.toString())
    assert(None == result)
  }

  "Function getOrElse" should "should retrive the inner value of a some" in {
    val result = (Some(2)).getOrElse(0)
    assert(2 == result)
  }

  it should "return the default value if inner value is none" in {
    val result = None.getOrElse(0)
    assert(0 == result)
  }


  "Function flatmap" should "operate on a some" in {
    val ob = Some(2)
    val result = ob.flatMap(value => Some(value.toString()))
    assert(Some("2") == result)
  }

  it should "not operate on a none" in {
    val result = None.flatMap(value => Some(value.toString()))
    assert(None == result)
  }

  "Function orElse" should "return the first object if it is some" in {
    val ob = Some(2)
    val result = ob orElse None
    assert(ob == result)
  }

  it should "return the second object if it is none" in {
    val result = None orElse Some(2)
    assert(Some(2) == result)
  }

  "Function filter" should "return none if given none" in {
    val result = None.filter(_ => true)
    assert(None == result)
  }

  it should "return some if given some and predicate is true" in {
    val result = (Some(2)).filter(_ => true)
    assert(Some(2) == result)
  }

  it should "return None if given some and predicate is false" in {
    val result = (Some(2)).filter(_ => false)
    assert(None == result)
  }

  "Function variance" should "function with basic example" in {
    val s = Seq(2.0,4.0)
    val realVariance = Some(1.0)
    val result = Option.variance(s)
    assert(realVariance == result)
  }

  it should "fail if not able to get the mean" in {
    val s = Seq()
    assert(None == Option.variance(s))
  }

  "Function map2" should "use the two arguments if they are both some" in {
    val val1 = Some("Hello ")
    val val2 = Some("World")
    assert(Some("Hello World") == Option.map2(val1,val2)((v1,v2) => v1 + v2))
  }

  it should "not run if second one is None" in {
    val val1 = Some("Hello ")
    assert(None == Option.map2(val1,None)((v1,v2) => v1 + v2))
  }

  it should "not run if the first one is None" in {
    val val2 = Some("World")
    assert(None == Option.map2(None,val2)((v1: String,v2: String) => v1 + v2))
  }

  it should "not run if both are None" in {
    assert(None == Option.map2(None,None)((v1: String,v2: String) => v1 + v2))
  }

  "Function sequence" should "return the values of a non empty list of some values" in {
    val l = List(Some(1),Some(2))
    val expected = Some(List(1,2))
    val actual = Option.sequence(l)
    assert(expected == actual)
  }

  it should "return none if even a single element is none" in {
    val l = List(Some(1),None)
    val actual = Option.sequence(l)
    assert(None == actual)
  }

  it should "return some empty list if given an empty list" in {
    val l = List.empty : List[Option[Int]]
    val actual = Option.sequence(l)
    assert(Some(l) == actual)
  }
}
