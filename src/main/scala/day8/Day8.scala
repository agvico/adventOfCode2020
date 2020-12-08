package day8

import scala.annotation.tailrec
import scala.io.Source

object Day8 {
  def main(args: Array[String]): Unit = {
    val data = Source.fromFile("src/main/scala/day8/input").mkString

    val instructionSet: Array[(String, Int, Int)] = data.split("\n")
      .map(l => {
        val regex = "(.{3}) ([\\+-])(\\d+)".r
        val regex(instruction, sign, value) = l
        (instruction, 0, if(sign == "-") -(value.toInt) else value.toInt)
      })

    // part1
    println(runProgram(instructionSet.clone(),0,0))

    
    // part2
    var found = false
    var index = 0
    var acc = -1
    while(index < instructionSet.length && !found){
      val modProgram = instructionSet.clone()
      val currInstruction = modProgram(index)
      val result: (Int, Boolean) = currInstruction._1 match {
        case "jmp" => {
          modProgram(index) = ("nop", currInstruction._2, currInstruction._3)
          runProgram(modProgram, 0,0)
        }
        case "nop" => {
          modProgram(index) = ("jmp", currInstruction._2, currInstruction._3)
          runProgram(modProgram, 0,0)
        }
        case _ => (0, false)
      }
      found = result._2
      index += 1
      if(found) acc = result._1
    }

    println(acc)
  }



  /**
   * It runs the program, return true if the program finished correctly or false if an instruction is exectued twice (inifinite loop)
   * @param instructions
   * @param programCounter
   * @param acc
   * @return
   */
  @tailrec def runProgram(instructions: Array[(String, Int, Int)], programCounter: Int, acc: Int): (Int, Boolean) = {
    if(programCounter >= instructions.length){
      (acc, true)
    } else {
      val instruction = instructions(programCounter)
      if(instruction._2 != 0){
        (acc, false)
      } else {
        instructions(programCounter) = (instruction._1, instruction._2 + 1, instruction._3)
        instruction._1 match {
          case "acc" => {
            runProgram(instructions, programCounter + 1, acc + instruction._3)
          }
          case "jmp" => {
            runProgram(instructions, programCounter + instruction._3, acc)
          }
          case "nop" => {
            runProgram(instructions, programCounter + 1, acc)
          }
        }
      }
    }
  }

}
