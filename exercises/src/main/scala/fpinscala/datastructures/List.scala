package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = 
    l match {
    case Nil => Nil
    case Cons(h,t) => t
    }

  def setHead[A](l: List[A], h: A): List[A] = 
    l match {
    case Nil => Cons(h,Nil)
    case Cons(x,xs) => Cons(h,xs)
    }

  def drop[A](l: List[A], n: Int): List[A] = 
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(h,t) => drop(t,n-1)
    }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = 
    l match {
    case Nil => Nil
    case Cons(h,t) => if (f(h)) dropWhile(t,f) else l
    }
  def rev[A](l: List[A]): List[A] = {
    foldLeft(l, Nil: List[A])((acc: List[A], elem: A) => Cons(elem,acc))
  }

  def init[A](l: List[A]): List[A] = {
    def helper[A](acc: List[A],list: List[A]) : List[A] = {
      list match {
      case Cons(h,Cons(e,Nil)) => Cons(h,acc)
      case Cons(h,t) => helper(Cons(h,acc), t)
      case Nil => Nil}
    }
    val res = helper(Nil,l)
    foldLeft(res,List(): List[A])((acc,elem) => Cons(elem,acc))
  }

  def length[A](l: List[A]): Int = {
    def helper[A](ls: List[A], count: Int) : Int = {
      ls match {
        case Nil => count
        case Cons(h,t) => helper(t,count+1)
      }
    }
    helper(l,0)
  }

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    l match {
      case Nil => z
      case Cons(h,t) => foldLeft(t, f(z,h))(f)
    }
  }

  def sumFoldLeft(l: List[Int]) = {
    foldLeft(l,0)(_+_)
  }

  def productFoldLeft(l: List[Double]) = {
    foldLeft(l,1.0)(_*_)
  }

  def lengthFoldLeft[A](l: List[A]) = {
    foldLeft(l,0)((acc,_) => acc + 1)
  }

  def foldRightViaFoldLeft[A,B](l: List[A], z: B)(f: (A,B) => B): B = {
    foldLeft(rev(l), z)((acc,elem) => f(elem,acc))
  }

  def id[A](a: A) : A = 
    a

  def foldLeftViaFoldRight[A,B](l: List[A], z: B)(f: (B,A) => B): B = {
    val funky = foldRight(l,(id: B => B))((elem,acc) => ((bb: B) => acc(f(bb,elem))))
    funky(z)
  }

  def flatten[A](l: List[List[A]]) = {
    foldLeft(l, Nil: List[A])((acc,elem) => append(acc,elem))
  }

  def add1(l: List[Int]) = {
    @annotation.tailrec
    def adder(ls: List[Int], acc: List[Int]): List[Int] =
      ls match {
        case Nil => acc
        case Cons(h,t) => adder(t,Cons(h+1,acc))
      }
    rev(adder(l,Nil: List[Int]))
  }

  def doubleListToString(l: List[Double]) = {
    @annotation.tailrec
    def stringifier(ls: List[Double], acc: List[String]): List[String] = 
      ls match {
        case Nil => acc
        case Cons(h,t) => stringifier(t,Cons(h.toString(),acc))
      }
    rev(stringifier(l,Nil: List[String]))
  }

  def map[A,B](l: List[A])(f: A => B): List[B] = {
    @annotation.tailrec
    def helper[A,B](ls: List[A], f: A => B, acc: List[B]): List[B] =
      ls match {
        case Nil => acc
        case Cons(h,t) => helper(t,f,Cons(f(h),acc))
      }
    rev(helper(l,f,Nil: List[B]))
  }

  def filter[A](l: List[A])(f: A => Boolean): List[A] = 
    foldRight(l, Nil: List[A])((elem,acc) => f(elem) match { case true => Cons(elem,acc); case false => acc } )

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = 
    flatten(map(as)(f))

  def filterByFlatMap[A](l: List[A])(f: A => Boolean): List[A] = 
    flatMap(l)(elem => f(elem) match { case true => List(elem); case false => Nil})

  def addLists(a1: List[Int], a2: List[Int]): List[Int] = {
    def helper(ls1: List[Int], ls2: List[Int], acc: List[Int]): List[Int] =
      ls1 match {
        case Nil => acc
        case Cons(h1,t1) =>
          ls2 match {
            case Nil => acc
            case Cons(h2,t2) => helper(t1,t2,append(acc,List(h1+h2)))
          }
      }
    helper(a1,a2, Nil: List[Int])
  }

  def zipWith[A,B](a1: List[A], a2: List[B]): List[(A,B)] = {
    def helper(ls1: List[A], ls2: List[B], acc: List[(A,B)]): List[(A,B)] = 
      ls1 match {
        case Nil => acc
        case Cons(h1,t1) =>
          ls2 match {
            case Nil => acc
            case Cons(h2,t2) => helper(t1,t2, append(acc,List((h1,h2))))
          }
      }
    helper(a1,a2,Nil: List[(A,B)])
  }

  def hasSubSequence[A](sup: List[A], sub: List[A]): Boolean = {
    def helper[A](sup1: List[A], sub1: List[A], acc: List[A]): Boolean = {
      sup1 match {
        case Nil => false
        case Cons(h,t) =>
          acc match {
            case Nil => true
            case Cons(subH, subT) =>
              h == subH match {
                case false => helper(t,sub1,sub1)
                case true => 
                  subT match {
                    case Nil => true
                    case _ => helper(t,sub1,subT) 
                  }
                  
              }
          }
      }
    }
    helper(sup,sub,sub)
  }
}
