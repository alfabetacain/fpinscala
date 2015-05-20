package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](tree: Tree[A]): Int = {
    tree match {
      case Leaf(_) => 1
      case Branch(left,right) => 1 + size(left) + size(right)
    }
  }

  def maximum(tree: Tree[Int]): Int = {
    tree match {
      case Leaf(value) => value
      case Branch(left,right) => (maximum(left)) max (maximum(right))
    }
  }

  def depth[A](tree: Tree[A]): Int = {
    def helper[A](t: Tree[A], acc: Int): Int = {
      t match {
        case Leaf(_) => acc
        case Branch(left,right) => helper(left, acc + 1) max helper(right, acc + 1)
      }
    }
    helper(tree,1)
  }

  def map[A,B](tree: Tree[A])(f: A => B): Tree[B] = {
    tree match {
      case Leaf(value) => Leaf(f(value))
      case Branch(left,right) => Branch(map(left)(f), map(right)(f))
    }
  }

  def fold[A,B](tree: Tree[A])(l: A => B)(b: (B,B) => B): B = {
    tree match {
      case Leaf(value) => l(value)
      case Branch(left,right) => b(fold(left)(l)(b),fold(right)(l)(b))
    }
  }

  def maximumFold(tree: Tree[Int]): Int = {
    fold(tree)(acc => acc)(_ max _)
  }

  def sizeFold[A](tree: Tree[A]): Int = {
    fold(tree)(_ => 1)(_ + _ + 1)
  }

  def depthFold[A](tree: Tree[A]): Int = {
    fold(tree)(_ => 1)((a,b) => (a max b) + 1)
  }

  def mapFold[A,B](tree: Tree[A])(f: A => B): Tree[B] = {
    fold(tree)(l => Leaf(f(l)): Tree[B])((a,b) => Branch(a,b))
  }
}
