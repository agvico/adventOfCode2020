package day3

import scala.io.Source

object Day3 {
  def main(args: Array[String]): Unit = {

    val data = Source.fromFile("src/main/scala/day3/day3_data").getLines().toSeq.drop(1)     // ignore first line

    // part1
    var pos = 0
    val trees = data.map(s => {
      pos = (pos + 3) % s.length
      if(s.charAt(pos) == '#') 1 else 0
    }).sum

    println(trees)

    // part 2
    val positions: Array[(Int,Int)] = Array((1,1) , (3,1), (5,1) ,(7,1) ,(1,2))

    val solution = positions.map(x =>{
      pos = 0
      data.zipWithIndex
        .filter(i => (i._2 + 1) % x._2 == 0)
        .map(s => {
          pos = (pos + x._1) % s._1.length
          if (s._1.charAt(pos) == '#') 1 else 0
        }).sum
    }).product

    println(solution)

  }
}
