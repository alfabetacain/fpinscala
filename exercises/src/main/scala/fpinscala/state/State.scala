package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (num1, rng1) = rng.nextInt
    if (num1 < 0)
      (-(num1+1), rng1)
    (num1, rng1)
  }

  def betterDouble: Rand[Double] = 
    map(_.nextInt)(_ / (Int.MaxValue.toDouble + 1))

  def double(rng: RNG): (Double, RNG) = {
    val (value,rng2) = nonNegativeInt(rng)
    (value / (Int.MaxValue.toDouble + 1), rng2)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (int, rng2) = rng.nextInt
    val (double,rng3) = RNG.double(rng2)
    ((int,double),rng3)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (double,rng2) = RNG.double(rng)
    val (int,rng3) = rng2.nextInt
    ((double,int),rng3)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (double1,rng1) = double(rng)
    val (double2,rng2) = double(rng1)
    val (double3,rng3) = double(rng2)
    ((double1,double2,double3),rng3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def generate(count: Int, acc: List[Int], rng: RNG): (List[Int], RNG) = {
      if (count > 0) {
        val (num, rng1) = rng.nextInt
        generate(count-1, num :: acc, rng1)
      } else {
        (acc, rng)
      }
    }
    val (ls, rng1) = generate(count, List.empty, rng)
    (ls.reverse, rng1)
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng =>
      val (first, rng1) = ra(rng)
      val (second, rng2) = rb(rng1)
      (f(first, second), rng2)
  }

  def map2ViaFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    flatMap(ra){
      a =>
        flatMap(rb){
          b =>
            unit(f(a,b))
        }
    }
  }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2(ra,rb)((_,_))

  val randIntDouble: Rand[(Int,Double)] = 
    both(int,double)

  val randDoubleInt: Rand[(Double,Int)] =
    both(double,int)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    val seq = fs.foldLeft[Rand[List[A]]](unit(List.empty)){
      (acc, elem) => map2(elem, acc)(_ :: _)
    }
    map(seq)(_.reverse)
  }

  def _ints(count: Int): Rand[List[Int]] = 
    sequence(List.fill(count)(int))

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng =>
      val (a, rng1) = f(rng)
      g(a)(rng1)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(int){
      x => 
        val mod = x % n
        if (x + (n - 1) - mod >= 0)
          unit(mod)
        else
          nonNegativeLessThan(n)
    }
  }
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = 
    flatMap(a => State.unit(f(a)))

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = 
    flatMap(a => sb.flatMap(b => State.unit(f(a,b))))


  def flatMap[B](f: A => State[S, B]): State[S, B] = {
    State(s => {
      val (a,s1) = run(s)
      (f(a)).run(s1)
    })
  }

}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  def unit[A,S](a: A): State[S,A] =
    State(s => (a,s))

  def sequence[A,S](fs: List[State[S,A]]): State[S,List[A]] = {
    fs match {
      case h :: t => h.map2(sequence(t))(_ :: _)
      case Nil => State.unit(List())
    }
  }
  def modify[S](f: S => S): State[S,Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def get[S]: State[S, S] = State(s => (s,s))

  def set[S](s: S): State[S,Unit] = State(_ => ((),s))


  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    sequence(inputs.map(i => modify((s: Machine) => (i,s) match {
      case (_, Machine(_,0,_)) => s
      case (Coin,Machine(false,_,_)) => s
      case (Turn, Machine(true,_,_)) => s
      case (Coin, Machine(true,candy,coin)) => 
        Machine(false,candy,coin+1)
      case (Turn, Machine(false, candy, coin)) =>
        Machine(true,candy-1,coin)
    }))).flatMap(_ => get).map(s => (s.coins,s.candies))
    //sequence(inputs.map(i => {
     // modify((s: Machine) => 
      //  (i,s) match {
       //   case (Coin,Machine(false,candies,coins)) => Machine(true,candies,coins+1)
        //  case (Turn,Machine(true,candies,coins)) => Machine(false,candies-1,coins)
         // case (_,machine) => machine
       // })
     // }
   // ))
  }
}
    /*rng => {
      val (a,rng2) = f(rng)
      g(a)(rng2)
    }*/
