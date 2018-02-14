val f: String => String = {
  case "ping" => "pong"
}

f("ping")
//f("asd")

/* Partial Functions */

val pf: PartialFunction[String, String] = {
  case "ping" => "pong"
}

pf("ping")
pf.isDefinedAt("abc")


/*
trait PartialFunction[-A,+R] extends Function1[-A, +R] {
  def apply(x:A)
  def isDefinedAt(x:A):Boolean
}
*/
