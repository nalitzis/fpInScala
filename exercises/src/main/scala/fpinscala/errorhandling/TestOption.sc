import fpinscala.errorhandling.Option
import fpinscala.errorhandling.Some
import fpinscala.errorhandling.None

val myList = Vector(3,4,3.3,3.9,3.8,3.4)
Option.variance(myList)
Option.mean(myList)

Option.absO(Some(-3))

val optString = Some("String")
val optInt = Some(3)

Option.map2(optString, optInt)((str, n) => str + n)

val listOfOptions = List(Some(2), Some(4), Some(-3))
val listOfOptionsNone = List(Some(2), Some(4), None, Some(-3))
Option.sequence(listOfOptions)
Option.sequence(listOfOptionsNone)

val listOfInt = List(12,4,2,6)

Option.traverse(listOfInt)(x => if (x > 3) Some(x) else None)

Option.trav2(listOfInt)(x => if (x > 3) Some(x) else None)