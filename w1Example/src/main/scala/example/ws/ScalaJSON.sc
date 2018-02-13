/*Representation of JSON in Scala*/
abstract class JSON
case class JSeq(elems: List[JSON]) extends JSON
case class JObj(binding: Map[String, JSON]) extends JSON
case class JNum(num: Double) extends JSON
case class JStr(str: String) extends JSON
case class JBool(b: Boolean) extends JSON
case object JNull extends JSON


type JBinding = (String, JSON)

trait Map[Key, Value] extends (Key => Value)

trait Seq[Elem] extends (Int => Elem)
