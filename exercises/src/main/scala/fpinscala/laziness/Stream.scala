package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def toList: List[A] = 
   this match {
     case Cons(h,t) => h() :: t().toList
     case Empty => List.empty
   } 
  

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }
  def take(n: Int): Stream[A] = {
    def go(nn: Int,s: Stream[A]): Option[(A,(Int,Stream[A]))] = 
      if (nn > 0) 
        s match {
          case Cons(h,t) => Some((h(),(nn-1,t())))//cons(h(),t().take(n-1))
          case Empty => None
        }
      else None
    unfold((n,this))((go _).tupled)
  }

  def drop(n: Int): Stream[A] = 
    if (n > 0) 
      this match {
        case Cons(h,t) => t().drop(n-1)
        case Empty => Empty
      }
    else this

  def takeWhile(p: A => Boolean): Stream[A] = 
    unfold(this) {
      case Cons(h,t) => 
        val hh = h()
        if (p(hh)) Some((hh,t())) else None
      case _ => None
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] = 
    unfold((this,s2)) {
      case (Cons(h1,t1),Cons(h2,t2)) => Some(((Some(h1()),Some(h2())),(t1(),t2())))
      case (_,Cons(h,t)) => Some(((None,Some(h())),(empty[A],t())))
      case (Cons(h,t),_) => Some(((Some(h()),None), (t(),empty[B])))
      case _ => None
    }

  def forAll(p: A => Boolean): Boolean = 
    foldRight(true)((a,b) => p(a) && b)

  def headOption: Option[A] = 
    foldRight(None: Option[A])((a,b) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] = {
    def go(l: Stream[A]): Option[(B,Stream[A])] = {
      l match {
        case Empty => None
        case Cons(head,tail) => Some((f(head()),tail()))
      }
    }
    unfold(this)(go)
  }

  def filter(p: A => Boolean): Stream[A] = 
    foldRight(empty[A])((a,b) => if (p(a)) cons(a,b) else b)

  def append[B >: A](s: => Stream[B]): Stream[B] = 
    foldRight(s: Stream[B])((a,b) => cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] = 
    foldRight(empty[B])((a,b) => f(a) append(b))

  def startsWith[B](s: Stream[B]): Boolean = {
    zipAll(s).takeWhile(_._2.isEmpty) forAll {
      case (h,hh) => h == hh
    }
  }

  def tails: Stream[Stream[A]] = {
    unfold(this) {
      case Empty => None
      case s => Some(s, s drop 1)
    } append Stream(empty)
  }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

    val ones: Stream[Int] = unfold(1)(_ => Some(1,1))
  def constant[A](a: A): Stream[A] = {
    unfold(a)(_ => Some(a,a))
  }
  def from(n: Int): Stream[Int] = 
    unfold(n)(value => Some(value,value+1))
  def fibs(a: Int,b: Int): Stream[Int] = {
    unfold((0,1))({ case (a,b) => Some(a,(b,a+b))})
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) map({ case (value,state) => cons(value,unfold(state)(f))}) getOrElse(empty[A])
  }
}
