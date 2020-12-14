package day14

import scala.collection.mutable.Map
import scala.io.Source

object Day14 {
  def main(args: Array[String]): Unit = {
    val data = Source.fromFile("src/main/scala/day14/input").getLines().toSeq

    val memory: Map[Long, Long] = Map.empty
    val regex = "(mem\\[)(\\d+)(\\])".r
    var mask: String = ""

    // part1
    data.foreach(s => {
      val instruction = s.split(" = ")
      instruction(0) match {
        case "mask" => mask = instruction(1)
        case _ => {
          val regex(_,memAddress,_) = instruction(0)
          val bits = instruction(1).toInt.toBinaryString
          val trailingZeroes = (for(i <- 1 to 36 - bits.length) yield '0').mkString
          val numberBits = trailingZeroes ++ bits
          val newNumber = mask.zip(numberBits).map{e => {
            e._1 match {
              case '0' => '0'
              case '1' => '1'
              case 'X' => e._2
            }
          }}.mkString
          memory += (memAddress.toLong -> java.lang.Long.parseLong(newNumber,2))
        }
      }
    })
    println(memory.map(i => BigInt(i._2)).sum)

    // part2
    memory.clear()
    data.foreach(s => {
      val instruction = s.split(" = ")
      instruction(0) match {
        case "mask" => mask = instruction(1)
        case _ => {
          val regex(_,memAddress,_) = instruction(0)
          val memAddressBit = memAddress.toInt.toBinaryString
          val number = instruction(1).toInt
          val trailingZeroes = (for(i <- 1 to 36 - memAddressBit.length) yield '0').mkString
          val bitsAddress = trailingZeroes ++ memAddressBit
          val newAddress = mask.zip(bitsAddress).map{e => {
            e._1 match {
              case '0' => e._2
              case '1' => '1'
              case 'X' => 'X'
            }
          }}.mkString

          getMemoryAddresses(newAddress, Seq.empty).foreach(m => memory += (m -> number))
        }
      }
    })

    println(memory.map(i => BigInt(i._2)).sum)
  }


  
  def getMemoryAddresses(mask: String, acc: Seq[Long]): Seq[Long] = {
    mask.find(_ == 'X') match {
      case Some(value) => {
        val res = for(i <- Array("0","1")) yield {
          getMemoryAddresses(mask.replaceFirst("X",i), acc)
        }
        res.reduce(_ ++ _)
      }
      case None => java.lang.Long.parseLong(mask, 2) +: acc

    }
  }
}
