package day13

import scala.io.Source
import scala.util.{Success, Try}

object Day13 {
  def main(args: Array[String]): Unit = {
    val data = Source.fromFile("src/main/scala/day13/input").getLines().toSeq
    val arriveTime = data.head.toInt

    // part1
    val buses = data(1).split(",").filter(_ != "x").map(_.toInt)
    val minutesToWait = buses.map(b => (b, b - (arriveTime % b)) )
    val minWaitTime = minutesToWait.minBy(_._2)
    println(minWaitTime._1 * minWaitTime._2)

    // part2
    val buses2 = data(1).split(",").zipWithIndex.filter(_._1 != "x").map(i => (i._1.toInt, - i._2) ) //.map(_.toInt)
    val a = buses2.map(_._2.toLong)
    val m = buses2.map(_._1.toLong)
    val crt = ChineseRemainderTheorem.crt(a, m)
    println(crt(0))


  }



}
