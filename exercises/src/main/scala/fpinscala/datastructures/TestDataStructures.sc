import fpinscala.datastructures.Cons
import fpinscala.datastructures.List
import fpinscala.datastructures.Nil

val myList : List[Int] = Cons(1, Cons(2, Cons(3, Nil)))

List.reverse(myList)

List.append(4, myList)
List.append2(List.append(4, myList), myList)

List.map[Int, String](myList)("[" + _ + "]!")

List.addOne(myList)

List.flatMap(myList)(n => List(n, n))

List.map(myList)(n => List(n, n))