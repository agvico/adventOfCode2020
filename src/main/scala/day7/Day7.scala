package day7

import scala.io.Source

object Day7 {
  def main(args: Array[String]): Unit = {
    val data = Source.fromFile("src/main/scala/day7/input").mkString

    // Part 1
    val rules: Map[String, Array[(Int,String)]] = data.split("\n")
      .map(l => {
        val keyvalue = l.split(" bags contain ")
        val key = keyvalue(0)
        val values = keyvalue(1).substring(0,keyvalue(1).length-1)
          .split(", ")
          .map(elem => {
            if(elem != "no other bags") {
              val regex = "(\\d) (.*)".r
              val regex(num, bagType) = elem
              (num.toInt," bags| bag".r.replaceFirstIn(bagType,""))
            } else {
              (0, "noBag")
            }
          })
        (key -> values)
      }).toMap


    var number = 0
    for(bag <- rules) {
     number += inspectBagForShinyBag(rules, bag._1, "shiny gold")
    }

    println(number)

    // part2
    println(numberOfBags(rules, "shiny gold"))
  }


  /**
   * Returns true if the bag can hold a shiny gold bag
   * @param r
   * @param bag
   * @return
   */
  def inspectBagForShinyBag(r: Map[String, Array[(Int, String)]], bag: String, bagToFind: String): Int = {
    if(r(bag).head._2 equals "noBag"){
      0
    } else {
      r(bag).map(b => {
        if (b._2 equals bagToFind) {
          1
        } else {
          inspectBagForShinyBag(r, b._2, bagToFind)
        }
      }).find(_ == 1) match {
        case Some(value) => 1
        case None => 0
      }
    }
  }

  /**
   * Returns the number of bags inside the given one.
   * @param r
   * @param bag
   * @return
   */
  def numberOfBags(r: Map[String, Array[(Int, String)]], bag: String): Int = {
    if (r(bag).head._2 equals "noBag") {
      0
    } else {
      val suma = r(bag).map(b => {
        b._1 + (b._1 * numberOfBags(r, b._2))
      })
      suma.sum
    }
  }
}
