package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{Executors,ExecutorService}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

case class Prop(run: (Int, TestCases, RNG) => Result) {
  def &&(p: Prop): Prop = {
    Prop{
      (max, count, rng) =>
        run(max, count, rng) match {
          case Passed => p.run(max, count, rng)
          case Proved => p.run(max, count, rng)
          case x => x
        }
    }
  }
  def ||(p: Prop): Prop = 
    Prop{
    (max, count, rng) =>
      run(max, count, rng) match {
        case Falsified(reason, successes) =>
          p.tag(reason).run(max, count, rng)
        case _ => Passed
      }
    }

  def tag(tag: String): Prop = {
    Prop {
      (max, count, rng) => 
        run(max, count, rng) match {
          case Falsified(reason, successes) => Falsified(s"$tag\n" + reason, successes)
          case Passed => Passed
          case Proved => Proved
        }
    }
  }
}

sealed trait Result {
  def isFalsified: Boolean
  type FailedCase = String
  type SuccessCount = Int
}
case object Passed extends Result {
  def isFalsified = false
}
case class Falsified(failure: String, successes: Int) extends Result {
  def isFalsified = true
}
case object Proved extends Result {
  def isFalsified = false
}

object Prop {
  type MaxSize = Int
  def run(p: Prop, maxSize: Int = 100, testCases: Int = 100, rng: RNG = RNG.Simple(System.currentTimeMillis)): Unit = {
    p.run(maxSize, testCases, rng) match {
      case Falsified(reason, successes) =>
        println(s"! Falsified after $successes passed tests:\n $reason")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
      case Proved =>
        println(s"+ OK, proved property.")
    }
  }
  def forAll[A](gen: SGen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      val casesPerSize = (n + (max - 1)) / max
      val props: Stream[Prop] = 
        Stream.from(0).take((n min max) + 1).map(i => forAll(gen(i))(f))
      val prop: Prop = 
        props.map(p => Prop { (max, _, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
      prop.run(max, n, rng)
  }
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = { Prop{
    (msg, n, rng) => randomStream(gen)(rng).zipWith(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch { case e: Exception => Falsified(buildMsg(a, e), i)}
    }.find(_.isFalsified).getOrElse(Passed)
  }}
  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] = {
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))
  }
  def buildMsg[A](s: A, e: Exception): String = {
    s"test case: $s\n" +
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
  }

  def check(p: => Boolean): Prop = Prop { (_,_,_) =>
    if (p) Proved else Falsified("()", 0)
  }
    
  type TestCases = Int
}

object Gen {
  def unit[A](a: => A): Gen[A] = 
    Gen(State.unit(a))

  def boolean: Gen[Boolean] = {
    val generator = choose(0, 2)
    Gen(generator.sample.map(x => if (x > 0) true else false))
  }

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
    Gen(State.sequence(List.fill(n)(g.sample)))
  }
  def listOf[A](g: Gen[A]): SGen[List[A]] = {
    SGen(g.listOfN(_))
  }
  def listOf1[A](g: Gen[A]): SGen[List[A]] = {
    SGen(n => g.listOfN(n max 1))
  }

  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive-start)))
  }

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = {
    boolean.flatMap(x => if (x) g1 else g2)
  }

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val threshold = g1._2.abs / (g2._2.abs + g2._2.abs)
    Gen(State(RNG.double).flatMap(x => if (x < threshold) g1._1.sample else g2._1.sample))
  }
}

case class Gen[A](sample: State[RNG, A]) {
  def flatMap[B](f: A => Gen[B]): Gen[B] = {
    Gen( 
      State{
        s =>
          val (res, s1) = sample.run(s)
          f(res).sample.run(s1)
      }
    )
  }

  def map[B](f: A => B): Gen[B] = {
    flatMap(x => Gen.unit(f(x)))
  }
  def listOfN(size: Gen[Int]): Gen[List[A]] = {
    size.flatMap{
      length =>
        Gen.listOfN(length, this)
    }
  }
  def listOfN(size: Int): Gen[List[A]] = {
    Gen.listOfN(size, this)
  }
  def unsized: SGen[A] = 
    SGen(_ => this)
}

case class SGen[A](forSize: Int => Gen[A]) {
  def apply(size: Int): Gen[A] = forSize(size)
  def map[B](f: A => B): SGen[B] =
    SGen(forSize(_).map(f))

  def flatMap[B](f: A => Gen[B]): SGen[B] = {
    SGen(forSize(_).flatMap(f))
  }
}

