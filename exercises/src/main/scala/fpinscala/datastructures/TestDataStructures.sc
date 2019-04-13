import fpinscala.datastructures.Cons
import fpinscala.datastructures.List
import fpinscala.datastructures.Nil

val myList : List[Int] = Cons(1, Cons(2, Cons(3, Nil)))

List.reverse(myList)

List.append(4, myList)
List.append2(List.append(4, myList), myList)