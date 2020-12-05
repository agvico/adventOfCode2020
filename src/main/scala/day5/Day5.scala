package day5

import scala.io.Source

object Day5 {

  def main(args: Array[String]): Unit = {
    val data = Source.fromFile("src/main/scala/day5/input").getLines()

    // Part1
    val seatsIDs = data.map(toBinary)
      .map(seat => seat._1 * 8 + seat._2).toSeq

    println(seatsIDs.max)

    // part2
    val idsSorted = seatsIDs.sorted
    val myId = idsSorted
      .zip(idsSorted.drop(1))
      .find(p => (p._1 + 1) != p._2) match {
      case Some(value) => value._2 - 1
      case None => - 1
    }
    println(myId)
  }

  def toBinary(s: String): (Int, Int) = {
    var aux = "[FL]".r.replaceAllIn(s,"0")
    aux = "[BR]".r.replaceAllIn(aux,"1")
    val number = Integer.parseInt(aux, 2)
    (number >> 3, number & 7) // 7 = "0000000111"
  }

}
