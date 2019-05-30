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
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val next = rng.nextInt
    if (next._1 == Int.MinValue) (Int.MaxValue, next._2)
    else (Math.abs(next._1), next._2)
  }

  //0 .. 1
  def double(rng: RNG): (Double, RNG) = {
    val next = nonNegativeInt(rng)
    (next._1.toDouble / Int.MaxValue.toDouble, next._2)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    ((nonNegativeInt(rng)._1, double(rng)._1), rng.nextInt._2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val next = intDouble(rng)
    ((next._1._2, next._1._1), next._2)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val next1 = double(rng)
    val next2 = double(next1._2)
    val next3 = double(next2._2)
    ((next1._1, next2._1, next3._1), next3._2)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = count match {
    case 0 => (List.empty, rng)
    case x => {
      val next = rng.nextInt
      (next._1 :: ints(x - 1)(next._2)._1, next._2)
    }
  }

  def doubles(count: Int)(rng: RNG): (List[Double], RNG) = count match {
    case 0 => (List.empty, rng)
    case x => {
      val next = double(rng)
      (next._1 :: doubles(x - 1)(next._2)._1, next._2)
    }
  }

  def doubleElegant(rng: RNG): (Double, RNG) = map(nonNegativeInt)(x => x.toDouble / Int.MaxValue.toDouble)(rng)

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a,b), rng3)
    }
  }

  val doubleIntElegant : Rand[(Double, Int)] = map2(double, nonNegativeInt)((a,b) => (a,b))
  val intDoubleElegant: Rand[(Int, Double)] = map2(nonNegativeInt, double)((a,b) => (a,b))

  def both[A,B](ra: Rand[A], rb: Rand[B]) : Rand[(A,B)] = map2(ra,rb)((_,_))

  val doubleIntElegant2 : Rand[(Double, Int)] = both(double, nonNegativeInt)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

  def intsElegant(count: Int)(rng: RNG): Rand[List[Int]] = sequence(List.fill(count)(int))

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val (a, rng1) = f(rng)
      g(a)(rng1)
    }
  }

  def nonNegativeLessThan(n: Int) : Rand[Int] = {
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n-1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }
  }

  def mapAsFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s) { a => unit(f(a)) }

  def map2AsFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    flatMap(ra) {
      a => map(rb) {
        b => f(a, b)
      }
    }
  }
}

/*def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

 */

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = State {
    state => {
      val (a1, s1) = run(state)
      (f(a1), s1)
    }
  }

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = flatMap(a => sb.map(b => f(a, b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] = State {
    state => {
      val (a, s) = run(state)
      f(a).run(s)
    }
  }
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
