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
}

