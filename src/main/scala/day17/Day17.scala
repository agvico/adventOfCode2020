package day17

import scala.annotation.tailrec
import scala.io.Source

object Day17 {
  def main(args: Array[String]): Unit = {
    val data = Source.fromFile("src/main/scala/day17/input").getLines().toIndexedSeq


    // Part 1
    val dim = for{
      x <- data.indices
      y <- 0 until data(x).length
    } yield { (x, y, 0) -> (data(x).charAt(y) == '#') }

    val dimensionMap = dim.toMap
    val after = cycles(0, dimensionMap, 6)
    println(after.map(i => if(i._2) 1 else 0).sum)


    // Part2
    val dim2 = for{
      x <- data.indices
      y <- 0 until data(x).length
    } yield { (x, y, 0, 0) -> (data(x).charAt(y) == '#') }
    val dimensionMap2 = dim2.toMap

    val after2 = cyclesPart2(0, dimensionMap2, 6)
    println(after2.map(i => if(i._2) 1 else 0).sum)
  }


  @tailrec def cycles(cycle: Int, dimension: Map[(Int,Int,Int), Boolean], numTotalCycles: Int): Map[(Int,Int,Int), Boolean] = {
    if(cycle == numTotalCycles){
      dimension
    } else {
      // for all elements
      val newDimensionMap = dimension.flatMap(elem => {
        val coordinate = elem._1
        val v = elem._2
        for {
          x <- (coordinate._1 - 1) to (coordinate._1 + 1)
          y <- (coordinate._2 - 1) to (coordinate._2 + 1)
          z <- (coordinate._3 - 1) to (coordinate._3 + 1)
          if coordinate != (x,y,z)
        } yield {
          (x,y,z) -> applyRules((x,y,z), dimension)
        }
      })
      cycles(cycle + 1, newDimensionMap, numTotalCycles)
    }
  }



  def applyRules(coordinate: (Int, Int, Int), dimension: Map[(Int,Int,Int), Boolean]): Boolean = {
    val activeCube = dimension.get(coordinate) match {
      case Some(value) => value
      case None => false
    }

    val actives = for {
      x <- (coordinate._1 - 1) to (coordinate._1 + 1)
      y <- (coordinate._2 - 1) to (coordinate._2 + 1)
      z <- (coordinate._3 - 1) to (coordinate._3 + 1)
      if coordinate != (x,y,z)
    } yield {
      dimension.get((x, y, z)) match {
        case Some(value) => {  // Element found process folliwng the rules
            if(value) 1 else 0
        }
        case None => {  // element not found. Do not count
            0
        }
      }
    }
    val activeNeighbours = actives.sum
    if(activeCube){
        activeNeighbours == 2 || activeNeighbours == 3
    } else {
        activeNeighbours == 3
    }
  }

//  PART 2 FUNCTIONS

  @tailrec def cyclesPart2(cycle: Int, dimension: Map[(Int,Int,Int,Int), Boolean], numTotalCycles: Int): Map[(Int,Int,Int,Int), Boolean] = {
    if(cycle == numTotalCycles){
      dimension
    } else {
      // for all elements
      val newDimensionMap = dimension.flatMap(elem => {
        val coordinate = elem._1
        val v = elem._2
        for {
          x <- (coordinate._1 - 1) to (coordinate._1 + 1)
          y <- (coordinate._2 - 1) to (coordinate._2 + 1)
          z <- (coordinate._3 - 1) to (coordinate._3 + 1)
          w <- (coordinate._4 - 1) to (coordinate._4 + 1)
          if coordinate != (x,y,z,w)
        } yield {
          (x,y,z,w) -> applyRulesPart2((x,y,z,w), dimension)
        }
      })
      cyclesPart2(cycle + 1, newDimensionMap, numTotalCycles)
    }
  }


  def applyRulesPart2(coordinate: (Int, Int, Int, Int), dimension: Map[(Int,Int,Int,Int), Boolean]): Boolean = {
    val activeCube = dimension.get(coordinate) match {
      case Some(value) => value
      case None => false
    }

    val actives = for {
      x <- (coordinate._1 - 1) to (coordinate._1 + 1)
      y <- (coordinate._2 - 1) to (coordinate._2 + 1)
      z <- (coordinate._3 - 1) to (coordinate._3 + 1)
      w <- (coordinate._4 - 1) to (coordinate._4 + 1)
      if coordinate != (x,y,z,w)
    } yield {
      dimension.get((x, y, z, w)) match {
        case Some(value) => {  // Element found process folliwng the rules
          if(value) 1 else 0
        }
        case None => {  // element not found. Do not count
          0
        }
      }
    }
    val activeNeighbours = actives.sum
    if(activeCube){
      activeNeighbours == 2 || activeNeighbours == 3
    } else {
      activeNeighbours == 3
    }
  }


}
