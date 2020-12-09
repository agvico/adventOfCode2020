package day9

import scala.io.Source
import scala.math.Fractional.Implicits.infixFractionalOps

object Day9 {
  def main(args: Array[String]): Unit = {
    val data = Source.fromFile("src/main/scala/day9/input").getLines().map(_.toLong).toSeq

    // part1
    val v = find(data, 25, -1)
    println(v)

    //part 2 (not finished yet)

  }


  def find(numbers: Seq[Long], numInSum: Int, curValue: Long): Long = {
    if(numbers.length < numInSum){
      curValue
    } else {
      val currNumbers = numbers.slice(0,numInSum)
      val value = numbers(numInSum)

      val found = for{i <- currNumbers
          j <- currNumbers
          if(i != j && j + i == value)} yield value

      if(found.isEmpty){
        find(Seq.empty, numInSum, value)
      } else {
        find(numbers.tail, numInSum, curValue)
      }
    }
  }

  
}
