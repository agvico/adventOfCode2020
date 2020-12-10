package day10

import scala.annotation.tailrec
import scala.io.Source

object Day10 {
  def main(args: Array[String]): Unit = {
    val data = Source.fromFile("src/main/scala/day10/input").getLines().map(_.toInt).toIndexedSeq.sorted
    val data1 = 0 +: data :+ (data.last + 3)

    // part1
    val diffs = for {i <- 1 until data1.length} yield data1(i) - data1(i - 1)
    println(diffs.count(_ == 1) * diffs.count(_ == 3))

    // part2
    val paths = Array.fill(data1.length)(0L)
    paths(0) = 1L
    for{
      i <- data1.indices.drop(1)
      j <- 1 to 3
      if i - j >= 0
      if data1(i) - data1(i - j) <= 3
    }
      yield paths(i) += paths(i - j)

    println(paths.last)

  }



}
