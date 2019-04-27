package fpinscala.errorhandling


import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(x) => {
      println("map Some(" + x+ "), returning Some(f(" + x + "))")
      Some(f(x))
    }
    case None => {
      println("map None, returning None")
      None
    }
  }

  def getOrElse[B>:A](default: => B): B = this match {
    case Some(x) => x
    case None => default
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Some(x) => {
      println("flatMap Some(" + x+ "), returning f(" + x + ")")
      f(x)
    }
    case None => {
      println("flatMap None, returning None")
      None
    }
  }

  def orElse[B>:A](ob: => Option[B]): Option[B] = this match {
    case x => x
    case None => ob
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(x) => if (f(x)) this else None
    case None => None
  }
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = mean(xs).flatMap(m => myVariance(m, xs))

  //map is mapping : y =  x => (xi - m)^2
  //mean is doing sum(y)/N = ((x0 - m)^2 + (x1 - m)^2 + ... + (xn -m)^2) / xs.length
  def myVariance(m: Double, xs: Seq[Double]): Option[Double] = mean(xs.map(x => math.pow(x - m, 2)))

  def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f

  val absO = lift(math.abs)

  def map2[A,B,C](aWithOption: Option[A], bWithOption: Option[B])(f: (A, B) => C): Option[C] =
    aWithOption flatMap (a => bWithOption map (b => f(a, b)))

  def map2bis[A,B,C](aWithOption: Option[A], bWithOption: Option[B])(f: (A, B) => C): Option[C] =
    aWithOption.flatMap(a => bWithOption.map(b => f(a,b)))

  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => {
      println("sequence Nil")
      Some(Nil)
    }
    case x :: xs => {
      println("x :: xs")
      //flatMap(A => Option[B]): Option[B] so xx is A (not Option[A]) second part is Option[B] i.e Option[List[A]] B = List[A])
      //map(A => B) : Option[B] so ys is a list of A, xx :: ys is a list of A, it will return Option[List[A]]
      x.flatMap(xx => sequence(xs).map(ys => xx :: ys))
    }
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case x :: xs => map2(f(x), traverse(xs)(f))(_ :: _)
  }

  def trav2[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case x :: xs => f(x).flatMap(xx => trav2(xs)(f).map(yy => xx :: yy))
  }
}