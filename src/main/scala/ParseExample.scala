import scala.util.parsing.combinator._

object ParseUtil extends JavaTokenParsers {

  //this will be provided at runtime to configure the allowed types
  val listOfParsers = List((gregg, 'gregg), (robert, 'robert), (tony, 'tony))

  //turn the list into a useful Parser
  val parser = buildParser(listOfParsers)

  //base parsers
  def gregg = "gregg" | "Gregg" | "JanxSpirit"
  def robert = "robert" | "Robert" | "robovoyo"
  def tony = "tony" | "Tony" | "tonytam"

  //create a Parser[(String, Symbol)] given a String, Symbol
  def parserTup(p: Parser[String], s: Symbol): Parser[(String, Symbol)] = (p ^^ { r => (r, s) })

  //create a Parser[(String, Symbol)] given a List[(String, Symbol)]
  def buildParser(l: List[(Parser[String], Symbol)]) = l.map(p => parserTup(p._1, p._2)).reduceLeft((a, b) => a | b)

  def main(args: Array[String]) {
    //try it out
    outputResult("gregg")
    outputResult("Gregg")
    outputResult("robovoyo")
    outputResult("tony")
  }

  def outputResult(strToParse: String) = {
    println("Trying to parse %s..." format (strToParse))
    val parsed = parseAll(parser, strToParse).getOrElse(("nothing", 'unknown))
    println("...parsed %s of type %s" format (parsed._1, parsed._2))
    println
  }
}

