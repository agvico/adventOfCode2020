package day11

import scala.annotation.tailrec
import scala.io.Source

object Day11 {
  def main(args: Array[String]): Unit = {
    val data: IndexedSeq[String] = Source.fromFile("src/main/scala/day11/input").getLines().toIndexedSeq

    // part1
    val arrays = Array(data, IndexedSeq.empty[String])
    var i = 0
    while(! (arrays(0) equals arrays(1))){
      arrays((i+1) % 2) = modifySeatMap(arrays(i), 1)
      i = (i+1) % 2
    }
    println(arrays(0).mkString.count(_ == '#'))


    // part2
    arrays(0) = data
    i = 0
    while(! (arrays(0) equals arrays(1))){
      arrays((i+1) % 2) = modifySeatMap(arrays(i), 2)
      i = (i+1) % 2
    }
    println(arrays(0).mkString.count(_ == '#'))
  }


  def adjacentOccupiedPart1(i: Int, j: Int, data: IndexedSeq[String]): Int = {
    val adjacentes = for {
      x <- i-1 to i+1
      y <- j-1 to j+1
      if (x != i ||y != j) && x >= 0 && x < data.length && y >= 0 && y < data(0).length
    } yield if(data(x).charAt(y) == '#') 1 else 0

    adjacentes.sum
  }

  def adjacentOccupiedPart2(i: Int, j: Int, data: IndexedSeq[String]): Int = {
    val occupations = for {
      incX <- Array(0, 1, -1)
      incY <- Array(0, 1, -1)
      if (0, 0) != (incX, incY)
    } yield {
      findOccupied(i + incX ,j + incY ,data,incX,incY,false)
    }
    occupations.sum
  }

  def modifySeatMap(data: IndexedSeq[String], part: Int): IndexedSeq[String] = {
    data.indices.map(i => {
      data(i).indices.map(j => {
        data(i).charAt(j) match {
          case 'L' => val res = if(part == 1) adjacentOccupiedPart1(i,j,data) == 0 else adjacentOccupiedPart2(i, j, data) == 0
            if (res) {
              '#'
            } else {
              'L'
            }
          case '#' => val res = if(part == 1) adjacentOccupiedPart1(i,j,data) >= 4 else adjacentOccupiedPart2(i, j, data) >= 5
          if(res){
            'L'
          } else {
            '#'
          }
          case '.' => '.'
        }
      }).mkString
    })
  }

  @tailrec def findOccupied(x: Int, y: Int, data: IndexedSeq[String], incX: Int, incY: Int, found: Boolean): Int = {
    if(x < 0 ||  y < 0 || x >= data.length || y >= data(0).length){
      if(found) 1 else 0
    } else {
      data(x).charAt(y) match {
        case 'L' => findOccupied(-1,-1, data, incX, incY, false)  // position free, return 0
        case '#' => findOccupied(-1,-1, data, incX, incY, true)
        case '.' => findOccupied(x + incX, y + incY, data, incX, incY, false)
      }
    }
  }

}
