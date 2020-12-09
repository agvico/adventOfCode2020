package day9

import scala.io.Source
import scala.math.Fractional.Implicits.infixFractionalOps

object Day9 {
  def main(args: Array[String]): Unit = {
    val data = Source.fromFile("src/main/scala/day9/input").getLines().map(_.toLong).toSeq

    // part1
     val v1: Long = data.tails
                    .filter(_.length > 26)
                    .find(s => {
                      val seq = s.take(25).combinations(2)
                      val value = s(25)
                      !seq.exists(_.sum == value)
                    }) match {
                    case Some(value) => value(25)
                    case None => -1
                  }
    println(v1)

    //part 2 (not finished yet)
   val range = data.tails.flatMap(_.inits.find(_.sum == v1)).next().sorted
    println(range.head + range.last)

  }

  
}
