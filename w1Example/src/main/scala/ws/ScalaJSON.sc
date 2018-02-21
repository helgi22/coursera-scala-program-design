/*Representation of JSON in Scala*/
abstract class JSON
case class JSeq(elems: List[JSON]) extends JSON
case class JObj(binding: Map[String, JSON]) extends JSON
case class JNum(num: Double) extends JSON
case class JStr(str: String) extends JSON
case class JBool(b: Boolean) extends JSON
case object JNull extends JSON

/*

val data = JObj(Map(
  "firstName" -> JStr("John"),
  "lastName" -> JStr("Smith"),
  "address" -> JObj(Map(
    "streetAddress" -> JStr("21 2nd Street"),
    "state" -> JStr("NY"),
    "postalCode" -> JNum(10021)
  )),
  "phoneNumbers" -> JSeq(List(
    JObj(Map(
      "type" -> JStr("home"), "number" -> JStr("212 555-1234")
    )),
    JObj(Map(
      "type" -> JStr("fax"), "number" -> JStr("646 555-4567")
    ))
  ))
))
*/

/*Pattern Matching*/
def show(json:JSON):String=json match {
  case JSeq(elems) => "[" + (elems map show mkString ",")+ "]"
  case JObj(bindings) =>
    val assocs = bindings match {
    case (key,value) => "\"" + key + "\":" + show(value)
  }
    "{"+ (assocs  mkString ",") + "}"
  case JNum(num) => num.toString
  case JStr(str) =>  '\"' + str + '\"'
  case JBool(b) => b.toString
  case JNull => "null"
}

type JBinding = (String, JSON)
//JBinding => String The same as scala.Function1[JBinding,String]
trait Function1[-A,+R]{
  def apply(x : A): R
}


trait Map[Key, Value] extends (Key => Value)

trait Seq[Elem] extends (Int => Elem)
//That's why we can write elems(i)