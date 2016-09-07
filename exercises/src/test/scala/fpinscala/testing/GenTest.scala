import org.scalatest._
import org.scalatest.Assertions._
import fpinscala.testing._
import fpinscala.parallelism._
import java.util.concurrent._

class GenTest extends FlatSpec with Matchers {

  "Gen.choose" should "be able to generate numbers between 0 and 100" in {
    val g = Gen.choose(-10, 10)
    val maxProps = Prop.forAll(Gen.listOf1(g)) {
      ns =>
        val max = ns.max
        !ns.exists(_ > max)
    }
    Prop.run(maxProps)
  }

  "List.sorted" should "sort a list" in {
    val g = Gen.choose(-10, 10)
    val props = Prop.forAll(Gen.listOf(g)) {
      list =>
        val sorted = list.sorted
        list.isEmpty || list.tail.isEmpty || !sorted.zip(sorted.tail).exists { 
          case (x,y) => 
            x>y
        }
    }
    Prop.run(props)
  }

  "Parallel map" should "not distort result" in {
    val es: ExecutorService = Executors.newCachedThreadPool
    val p1 = Prop.check((Par.map(Par.unit(1))(x=>x))(es).get == Par.unit(2)(es).get)
    Prop.run(p1)
  }
}
