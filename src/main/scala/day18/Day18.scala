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


class ArithPrecedence extends JavaTokenParsers {

  def digit: Parser[Long] = "\\d+".r  ^^ {_.toLong}
  def sum = "+"
  def product = "*"

  def E = T ~ rep(product ~ T) ^^ {
    case d ~ expList => expList.foldLeft(d){ case (exp1,  _ ~ exp2) => exp1 * exp2}
  } | T

  def T = F ~ rep(sum ~ F) ^^ {
    case d ~ expList => expList.foldLeft(d){case (exp1, _ ~ exp2) => exp1 + exp2}
  } | F

  def F:Parser[Long] = "("~E~")" ^^ {case _ ~ exp ~ _ => exp} | digit
}


object Day18 {
  def main(args: Array[String]): Unit = {
    val data = Source.fromFile("src/main/scala/day18/input").getLines().toIndexedSeq
    val parser1 = new ArithNoPrecedence
    val parser2 = new ArithPrecedence

    // Part 1
    println(data.map(str => {
      parser1.parseAll(parser1.expression, str).get
    }).sum
    )

    // Part 2
    println(data.map(str => {
      parser2.parseAll(parser2.E, str).get
    }).sum
    )
  }

}
