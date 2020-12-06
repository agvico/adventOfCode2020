package day6

import scala.io.Source

object Day6 {
  def main(args: Array[String]): Unit = {
    val data = Source.fromFile("src/main/scala/day6/input").mkString
    val groups = "\n\n".r.split(data)

    // Part1
    val number = groups.map(_.replace("\n","").distinct.length).sum
    println(number)

    // Part2
    val numbers2 = groups.map(_.split("\n")
      .reduceLeft(_ intersect _).length
    ).sum

    println(numbers2)
  }
}
