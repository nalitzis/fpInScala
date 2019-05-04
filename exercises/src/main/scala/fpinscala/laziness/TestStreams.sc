import fpinscala.laziness.Stream

val myStream = Stream(1,2,3,4)
println(myStream)

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