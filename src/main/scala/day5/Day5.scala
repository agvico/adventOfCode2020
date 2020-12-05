package day5

import scala.io.Source

object Day5 {

  def main(args: Array[String]): Unit = {
    val data = Source.fromFile("src/main/scala/day5/input").getLines()

    // Part1
    val seatsIDs = data.map(toBinary)
      .map(l => {
        val row = Integer.parseInt(l.substring(0,7), 2)
        val col = Integer.parseInt(l.substring(7,l.length), 2)
        (row,col)
      }).map(seat => seat._1 * 8 + seat._2).toSeq

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


  def toBinary(s: String): String = {
    val aux = "[FL]".r.replaceAllIn(s,"0")
    "[BR]".r.replaceAllIn(aux,"1")
  }

}
