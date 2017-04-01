package fpinscala.chapter9

import language.higherKinds

trait JSON

object JSON {
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON
  
  // Exercise 9.9
  def jsonParser[Error, Parser[+_]](P: Parsers[Error, Parser]): Parser[JSON] = {
    import P._
    val spaces = char(' ').many.slice
    val quote = char('"') map (_.toString)
    val comma = char(',') map (_.toString)
    val colon = char(':') map (_.toString)
    val startObject = char('{') map (_.toString)
    val endObject = char('}') map (_.toString)
    val startArray = char('[') map (_.toString)
    val endArray = char(']') map (_.toString)
    val letter = regex("[a-zA-Z]".r)
    val digit = regex("\\d".r)
    val whitespace = regex("\\s".r)
    val jnull: Parser[JSON] = string("null") map (_ => JNull)
    val jnumber: Parser[JSON] = regex("[+-]?((\\d+\\.?\\d*)|(\\.\\d+)".r) map (s => JNumber(s.toDouble)) // s.toDouble could throw an exception, it should be handle with a try
    val jstring: Parser[JSON] = quote ** letter.many.slice ** quote map { case ((_, s), _) => JString(s) }
    val jbool: Parser[JSON] = regex("true|false".r) map (s => JBool(s.toBoolean)) // s.toBoolean could throw an exception, it should be handle with a try
    val literal: Parser[JSON] = jnull or jbool or jnumber or jstring
    def jvalue: Parser[JSON] = literal or jarray or jobject
    def jarray: Parser[JSON] = {
      val empty = startArray.map2(endArray) { case _ => JArray(IndexedSeq()) }
      empty or (jvalue.map2(comma)((v, c) => v).many.map2(jvalue)((vs, v) => v :: vs).map(vs => JArray(vs.toIndexedSeq)))
    }
    def jobject: Parser[JSON] = {
      val empty = startObject.map2(endObject) { case _ => JObject(Map()) }
      val jKeyValuePair: Parser[(String, JSON)] = {
        val key: Parser[String] = quote ** letter.many.slice ** quote map { case ((_, s), _) => s }
        key.map2(colon)((k, c) => k) ** jvalue
      }
      empty or (jKeyValuePair.map2(comma)((kvp, c) => kvp).many.map2(jKeyValuePair)((kvps, kvp) => kvp :: kvps).map(kvps => JObject(kvps.toMap))) // This will not handle duplicate keys properly
    }
    jarray or jobject
  }
}

object JSONExample extends App {
  val jsonTxt = """
{
  "Company name" : "Microsoft Corporation",
  "Ticker"  : "MSFT",
  "Active"  : true,
  "Price"   : 30.66,
  "Shares outstanding" : 8.38e9,
  "Related companies" : [ "HPQ", "IBM", "YHOO", "DELL", "GOOG" ]
}
"""

  val malformedJson1 = """
{
  "Company name" ; "Microsoft Corporation"
}
"""

  val malformedJson2 = """
[
  [ "HPQ", "IBM",
  "YHOO", "DELL" ++
  "GOOG"
  ]
]
"""

  // TODO: Run parser
}