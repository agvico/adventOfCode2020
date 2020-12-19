package day18

import scala.io.Source
import scala.util.parsing.combinator._

class ArithNoPrecedence extends JavaTokenParsers {
  def digit: Parser[Long] = "\\d+".r  ^^ {_.toLong}  | parenthesis
  def operator: Parser[String] = "+" | "*"
  def parenthesis:Parser[Long] = "("~expression~")" ^^ {case _ ~ exp ~ _ => exp}
  def expression: Parser[Long] = digit ~ rep(operator ~ digit)  ^^ {
    case d ~ expressions => expressions.foldLeft(d){
      case (d1, "+" ~ d2) => d1 + d2
      case (d1, "*" ~ d2) => d1 * d2
    }
  }
}


object Day18 {
  def main(args: Array[String]): Unit = {
    val data = Source.fromFile("src/main/scala/day18/input").getLines().toIndexedSeq
    val parser1 = new ArithNoPrecedence

    // Part 1
    println(data.map(str => {
      parser1.parseAll(parser1.expression, str).get
    }).sum
    )

  }

}
