/*Representation of JSON in Scala*/
abstract class JSON
case class JSeq(elems: List[JSON]) extends JSON
case class JObj(binding: Map[String, JSON]) extends JSON
case class JNum(num: Double) extends JSON
case class JStr(str: String) extends JSON
case class JBool(b: Boolean) extends JSON
case object JNull extends JSON


type JBinding = (String, JSON)
//JBinding => String The same as scala.Function1[JBinding,String]
trait Function1[-A,+R]{
  def apply(x : A): R
}


trait Map[Key, Value] extends (Key => Value)

trait Seq[Elem] extends (Int => Elem)
//That's why we can write elems(i)

val f: String => String = { case "ping" => "pong"}
f("ping")
//f("asd")

val oh: (Int, String) => Boolean = {case (1,"one") => true}
oh(1,"one")

/* Partial Functions */
val pf1: PartialFunction[String, String] = {case "ping" => "pong"}
pf1("ping")
pf1.isDefinedAt("abc")

val pf2: PartialFunction[Boolean, String] ={case true => "one"; case false => "zero"}
pf2(true)
pf2(false)
pf2.isDefinedAt(false)
pf2.andThen[String](pf1).isDefinedAt(false)

/*
trait PartialFunction[-A,+R] extends Function1[-A, +R] {
  def apply(x:A)
  def isDefinedAt(x:A):Boolean
}
*/
