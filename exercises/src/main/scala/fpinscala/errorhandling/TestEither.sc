import fpinscala.errorhandling.Either
import fpinscala.errorhandling.Right
import fpinscala.errorhandling.Left

val m = List(Right(3), Right(4), Right(7))

Right(3).map2(Right(4))((x, y:Int) => x * y)