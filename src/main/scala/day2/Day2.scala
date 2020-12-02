package day2

import scala.io.Source

object Day2 {

  def main(args: Array[String]): Unit = {

    def lines = Source.fromFile("src/main/scala/day2/day2_input.txt").getLines()

    // part1
    val matches = lines.map(s => {
      val v = s.split(" ")
      val range = v(0).split("-").map(_.toInt)
      val char = v(1).charAt(0)
      val regex = ("[^" + char + "]").r
      val len = regex.replaceAllIn(v(2), "").length
      if(len >= range(0) && len <= range(1)) 1 else 0
    }).sum

    println(matches)


    // Part 2
    val matches2 = lines.map(s => {
      val v = s.split(" ")
      val range = v(0).split("-").map(_.toInt)
      val char = v(1).charAt(0)
      val regex1 = (".{" + (range(0) - 1) + "}" + char + ".*").r
      val regex2 = (".{" + (range(1) - 1) + "}" + char + ".*").r
      if(regex1.matches(v(2)) != regex2.matches(v(2))) 1 else 0
    }).sum

    println(matches2)
  }
}
