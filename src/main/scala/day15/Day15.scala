package day15

import scala.annotation.tailrec
import scala.collection.mutable.Map

object Day15 {

  def main(args: Array[String]): Unit = {
    val inputNumber = Array(14,1,17,0,3,20)
    val map: Map[Int,Int] = Map.empty
      inputNumber.indices.dropRight(1).foreach(i => map += (inputNumber(i) -> (i+1)))
    // Part 1
     println(game(inputNumber.length + 1, inputNumber.last, map, 2021))

    // Part 2
    val t = System.currentTimeMillis()
    println(game(inputNumber.length + 1, inputNumber.last, map, 30000001))
    println(System.currentTimeMillis() - t)
  }

  @tailrec def game(turn: Int, number: Int, map: Map[Int, Int], finalTurn: Int): Int = {
    if(turn == finalTurn){
      number
    } else {
      val newNum = map.get(number) match {
        case Some(previousTurn) => turn - 1 - previousTurn
        case None => 0
      }
      map += (number -> (turn - 1))
      game(turn + 1, newNum, map, finalTurn)
    }
  }
}
