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
      .map(_.toCharArray)
      .reduceLeft((x,y) =>
        (x.filter(y.contains(_)) ++ y.filter(x.contains(_))).distinct
      ).length
    ).sum

    println(numbers2)
  }
}
