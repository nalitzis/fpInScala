import fpinscala.state.RNG.Simple
import fpinscala.state.{RNG, State}

val s = RNG
val p = Simple(42)

s.int(p)
p

p.nextInt
p.nextInt._2.nextInt
s.nonNegativeInt(p)._1

s.double(p)._1
s.intDouble(p)._1
s.doubleInt(p)._1

s.double3(p)._1

s.ints(3)(p)._1
s.doubles(3)(p)._1

s.doubleElegant(p)._1
s.map2(s.nonNegativeInt, s.doubleElegant)((a,b) => (a,b))(p)._1
s.doubleIntElegant(p)._1
s.intDoubleElegant(p)._1
s.doubleIntElegant2(p)._1

s.intsElegant(4)(p)(p)._1

s.map2AsFlatMap(s.nonNegativeInt, s.doubleElegant)((a,b) => (a,b))(p)._1

val st = State(s.intDouble)
val st2 = State(s.int)
st.map(x => (x._1 + 2, x._2 + 2)).run(p)
st.map2(st2)((a,b) => (a,b)).run(p)