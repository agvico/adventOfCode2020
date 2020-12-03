package day3

import scala.io.Source

object Day3 {
  def main(args: Array[String]): Unit = {

    // Part1
    val trees = Source.fromFile("src/main/scala/day3/day3_data")
      .getLines()
      .toSeq
      .zipWithIndex
      .filter(_._2 != 0)
      .map(s => {
      if(s._1.charAt((s._2 * 3) % s._1.length) == '#') 1 else 0
    }).sum
    println(trees)

    // Part2
    val sol = Array((3,1),(1,1),(5,1),(7,1),(1,2)).map(x => {
      Source.fromFile("src/main/scala/day3/day3_data")
        .getLines()
        .toSeq
        .zipWithIndex.filter(_._2 != 0)
        .filter(_._2 % x._2 == 0)
        .map(s => {
          val pos = ((s._2 / x._2) * x._1) % s._1.length
          if (s._1.charAt(pos) == '#') 1 else 0
        }).sum

    }).product

    println(sol)



  }
}
