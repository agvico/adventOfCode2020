package day12

import scala.annotation.tailrec
import scala.io.Source

object Day12 {
  def main(args: Array[String]): Unit = {
    val data = Source.fromFile("src/main/scala/day12/input").getLines()

    val instructions = data.map(s => {
      val regex = "([NSWERLF])(\\d+)".r
      val regex(ins, num) = s
      (ins, num.toInt)
    }).toSeq

    // Part1
    val shipPosition = ((0,0), (1,0))  // (x,y,facePosition)
    val finalPos = instructions.foldLeft(shipPosition)((shipPos, instruction) => {
      val ship = shipPos._1
      val face = shipPos._2
      val inst = instruction._1
      val number = instruction._2
      inst.charAt(0) match {
        case 'N' => ((ship._1, ship._2 + number), face)
        case 'S' => ((ship._1, ship._2 - number), face)
        case 'E' => ((ship._1 + number, ship._2), face)
        case 'W' => ((ship._1 - number, ship._2), face)
        case 'R' => {
          val rotations = number / 90
          var faceAux = face
          var i = 0
          while(i < rotations) {
            faceAux = (faceAux._2, -faceAux._1)
            i+=1
          }
          (ship, faceAux)
        }
        case 'L' => {
          val rotations = number / 90
          var faceAux = face
          var i = 0
          while (i < rotations) {
            faceAux = (-faceAux._2, faceAux._1)
            i += 1
          }
          (ship, faceAux)
        }
        case 'F' => ((ship._1 + number * face._1, ship._2 + number * face._2) , face)
      }
    })

    val finalShipPos = finalPos._1
    println(math.abs(finalShipPos._1) + math.abs(finalShipPos._2))


    // Part 2
    val finalShipPosition = runInstructions( (10,1), (0,0), instructions )
    println(math.abs(finalShipPosition._1) + math.abs(finalShipPosition._2))
  }


  @tailrec def runInstructions(waypoint: (Int, Int), ship: (Int, Int), instructions: Seq[(String, Int)]): (Int, Int) = {
    if(instructions.isEmpty){
      ship
    } else {
      val instruction = instructions.head
      val inst = instruction._1
      val number = instruction._2
      inst.charAt(0) match {
        case 'N' => runInstructions((waypoint._1, waypoint._2 + number), ship, instructions.tail)
        case 'S' => runInstructions((waypoint._1, waypoint._2 - number), ship, instructions.tail)
        case 'E' => runInstructions((waypoint._1 + number, waypoint._2), ship, instructions.tail)
        case 'W' => runInstructions((waypoint._1 - number, waypoint._2), ship, instructions.tail)
        case 'R' => {
          val rotations = number / 90
          var diff = (waypoint._1 - ship._1, waypoint._2 - ship._2)
          var i = 0
          while(i < rotations) {
            diff = (diff._2, -diff._1)
            i+=1
          }
           runInstructions((ship._1 + diff._1, ship._2 + diff._2), ship, instructions.tail)
        }
        case 'L' => {
          val rotations = number / 90
          var diff = (waypoint._1 - ship._1, waypoint._2 - ship._2)
          var i = 0
          while (i < rotations) {
            diff = (-diff._2, diff._1)
            i += 1
          }
          runInstructions((ship._1 + diff._1, ship._2 + diff._2), ship, instructions.tail)
        }
        case 'F' =>  {
          val diff = (waypoint._1 - ship._1, waypoint._2 - ship._2)
          val newWaypoint = (waypoint._1 + diff._1 * number, waypoint._2 + diff._2 * number)
          val newShip = (ship._1 + diff._1 * number, ship._2 + diff._2 * number)
          runInstructions(newWaypoint, newShip, instructions.tail)
        }
      }
    }
  }

}
