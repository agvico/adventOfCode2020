package day5

import scala.io.Source

object Day5 {

  def main(args: Array[String]): Unit = {
    val data = Source.fromFile("src/main/scala/day5/input").getLines()

    // Part1
    val seatsIDs = data.map(line => {
      val row: Int = Integer.parseInt(line.substring(0,7).map(char => if(char == 'F') '0' else '1' ), 2)
      val column: Int = Integer.parseInt(line.substring(7,line.length).map(char => if(char == 'L') '0' else '1'), 2)
      (row, column)
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

}
