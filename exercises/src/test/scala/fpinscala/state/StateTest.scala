import org.scalatest._
import org.scalatest.Assertions._
import fpinscala.state._

class StateTest extends FlatSpec {

  "RNGSimple" should "generate the same random number when using the same seed" in {
    val rng = RNG.Simple(0)
    val (num1, _) = rng.nextInt
    val (num2, _) = rng.nextInt
    assert(num1 == num2)
  }

  "function nonNegative" should "return a positive number" in {
    val rng = RNG.Simple(0)
    val (num1, _) = RNG.nonNegativeInt(rng)
    assert(num1 >= 0)
  }

  "function double" should "return a double between 0 and 1, 1 not included" in {
    val rng = RNG.Simple(0)
    val (dub, _) = RNG.double(rng)
    assert(dub < 1.0 && dub >= 0.0)
  }

  "function betterDouble" should "return the same as double given the same seed" in {
    val rng = RNG.Simple(0)
    val (dub1, _) = RNG.double(rng)
    val (dub2, _) = RNG.betterDouble(rng)
    assert(dub1 == dub2)
  }

  "function map2" can "be used for returning an int, double tuple" in {
    val rng = RNG.Simple(0)
    val ((first, second), _) = RNG.map2(RNG.unit(1), RNG.unit(2.0)){(_,_)}(rng)
    assert(first == 1)
    assert(second == 2.0)
  }

  "function sequence" should "not change the order of random sequence when using unit RNG" in {
    val first = RNG.unit(1)
    val second = RNG.unit(2)
    val rng = RNG.Simple(0)
    val (list, _) = RNG.sequence(List(first, second))(rng)
    assert(list == List(1, 2))
  }

  "function nonNegativeLessThan" should "be able to generate random number lower than 10" in {
    val rng = RNG.Simple(0)
    val (res, _) = RNG.nonNegativeLessThan(10)(rng)
    assert(res < 10)
  }
}
