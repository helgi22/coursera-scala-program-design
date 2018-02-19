//functional literal
val f = (i: Int) => List(s"pred=${i - 1}'", s"same=${i}", s"${i + 1}")

val list = List(5, 6, 7)
println(list.flatMap(f))
