import fpinscala.laziness.Stream

val myStream = Stream(1,2,3,4)
println(myStream)

println(myStream.foldRight(0)((a, b) => a + b))

myStream.toList
myStream.drop(1).toList
myStream.take(2).toList
myStream.takeWhile(x => x < 3).toList

myStream.exists(_ > 0)
myStream.forAll(_ > 1)
myStream.takeWhileFoldRight(x => x < 3).toList

// not evaluated here
val stream = myStream.map(x => x*2)
// here i need it, and then the stream is evaluated lazily
val h = stream.take(1).toList

Stream.constant(4).take(5).toList
Stream.from(4).take(6).toList

Stream.unfold(0)(x => if(x > 10) None else Some((x,x+1))).toList
(0 to 10).toList

Stream.fromByUnfold(4).take(6).toList

Stream.onesByUnfold.take(5).toList

myStream.takeByUnfold(2).toList