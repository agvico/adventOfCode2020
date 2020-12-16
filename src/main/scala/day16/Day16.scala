package day16

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

object Day16 {
  def main(args: Array[String]): Unit = {
    val data = Source.fromFile("src/main/scala/day16/input").getLines().mkString("\n")
    val rules: mutable.Map[String, Seq[(Int,Int)]] = mutable.Map.empty
    val ruleRegex = "([\\w\\s]*)(: )(\\d+)(-)(\\d+)( or )(\\d+)(-)(\\d+)".r
    val fields = data.split("\n\n")

    fields(0).split("\n").foreach(s =>{
      val ruleRegex(field, _, n1,_,n2,_,n3,_,n4) = s
      rules += (field -> List((n1.toInt,n2.toInt), (n3.toInt,n4.toInt)))
    })
    val myTicket = fields(1).split("\n")(1).split(",").map(_.toInt)
    val nearbyTickets = fields(2).split("\n").drop(1).map(_.split(",").map(_.toInt))

    // part 1
    val invalidValues = for{
      i <- nearbyTickets;
      j <- i
      if !isValid(j, rules)
    } yield {
      j
    }
    println(invalidValues.sum)

    // part2
    val validTickets = myTicket +: nearbyTickets.filter(_.forall(isValid(_,rules)))

    // get for each column the possible candidates:
    // Only one column will have a single variable. The remaining will be discarded iteratively
    val possibleCandidates = validTickets.transpose.map(col => {
      val candidates = col.map(num => {
        rules.toSeq.flatMap(rule => {
          if(rule._2.exists(v => num >= v._1 && num <= v._2)){
            Some(rule._1)
          } else {
            None
          }
        })
      })
      candidates.reduce(_ intersect _)
    })

    val vars = getVars(possibleCandidates.zipWithIndex, Seq.empty)
    println(vars.filter(_._2.startsWith("departure"))
      .map(i => myTicket(i._1).toLong)
      .product)
  }


  def isValid(value: Int, rules: mutable.Map[String, Seq[(Int,Int)]]): Boolean = {
    rules.values.exists(_.map(v => value >= v._1 && value <= v._2).reduce(_ || _))
  }

  @tailrec def getVars(candidates: Array[(Seq[String], Int)], result: Seq[(Int, String)]): Seq[(Int,String)] = {
    if(candidates.isEmpty){
      result
    } else {
      candidates.find(_._1.length == 1) match {
        case Some(value) => {
          val newc = candidates.map(i => (i._1.filter(_ != value._1.head),i._2))
          getVars(newc.filter(_._1.nonEmpty), (value._2,value._1.head) +: result)
        }
        case None => getVars(candidates, result)
      }
    }
  }

}
