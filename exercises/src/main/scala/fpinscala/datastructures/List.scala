package fpinscala.datastructures

trait List[+A] {
  def isEmpty : Boolean
}// `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] {
  override def isEmpty: Boolean = true
}// A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A] {
  override def isEmpty: Boolean = false
}

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

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] =
    l match {
      case Nil => throw new IllegalArgumentException("can't call tail on an empty list!")
      case Cons(_, t) => t
    }

  def setHead[A](l: List[A], h: A): List[A] =
    l match {
      case Nil => throw new IllegalArgumentException("can't set head on an empty list!")
      case Cons(_,t) => Cons(h, t)
    }

  def drop[A](l: List[A], n: Int): List[A] =
    l match {
      case Nil => if (n > 0) throw new IllegalArgumentException("can't drop from an empty list!") else l
      case Cons(_, t) => if (n > 0) drop(t, n - 1) else l
    }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Nil => Nil
      case Cons(h, t) => {
        if (f(h)) dropWhile(t, f)
        else Cons(h, dropWhile(t, f))
      }
    }

  def dropWhileCurried[A](l: List[A])(f: A=> Boolean) : List[A] =
    l match {
      case Nil => Nil
      case Cons(h, t) => {
        if (f(h)) dropWhileCurried(t)(f)
        else Cons(h, dropWhileCurried(t)(f))
      }
    }

  def init[A](l: List[A]): List[A] =
    l match {
      case Nil => Nil
      case Cons(h,t) => {
        if (t == Nil) Nil
        else Cons(h, init(t))
      }
    }

  def length[A](l: List[A]): Int =
    l match {
      case Nil => 0
      case Cons(_, t) => foldRight(t, 1)((_, acc) => acc + 1)
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B =
    l match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

  def sumFoldLeft(ns: List[Int]) = foldLeft(ns, 0)(_ + _)
  def prodFoldLeft(ns: List[Double]) = foldLeft(ns, 1.0)(_ * _)
  def lenghtFoldLeft[A](l: List[A]): Int =
    l match {
      case Nil => 0
      case Cons(_, t) => foldLeft(t, 1)((acc, _) => acc + 1)
    }

  def reverse[A](xs: List[A]) = foldLeft(xs, Nil: List[A])((accList : List[A], a: A) => Cons(a, accList))

  def append[A](a: A, xs: List[A]) = reverse(Cons(a, reverse(xs)))
  def append2[A](xs: List[A], ys: List[A]) = foldRight(xs, ys)(Cons(_,_))

  def map[A,B](l: List[A])(f: A => B): List[B] =
    l match {
      case Nil => Nil
      case Cons(h,t) => Cons(f(h), map(t)(f))
    }

  def addOne(xs: List[Int]) : List[Int] = map(xs)(n => n+1)

  def concat[A](l: List[List[A]]): List[A] = foldRight(l, Nil:List[A])(append)

  def flatMap[A,B](xs: List[A])(f: A => List[B]) = concat(map(xs)(f))
}
