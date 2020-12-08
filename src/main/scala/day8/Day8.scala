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
    println(programReplace(instructionSet, Array.empty, (0,false)))
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

  @tailrec def programReplace(remainingInstructions: Array[(String, Int, Int)], previousInstructions: Array[(String, Int, Int)], result: (Int, Boolean)): (Int, Boolean) = {
    if(remainingInstructions.isEmpty){
      result
    } else {
      val currInstruction = remainingInstructions.head
      if(currInstruction._1 != "acc") {
        val newProgram = currInstruction._1 match {
          case "jmp" => previousInstructions :+ ("nop", currInstruction._2, currInstruction._3) concat remainingInstructions.tail
          case "nop" => previousInstructions :+ ("jmp", currInstruction._2, currInstruction._3) concat remainingInstructions.tail
        }
        val output = runProgram(newProgram, 0, 0)
        if (output._2) {
          programReplace(Array.empty, Array.empty, output)
        } else {
          programReplace(remainingInstructions.tail, previousInstructions :+ currInstruction, output)
        }
      } else {
        programReplace(remainingInstructions.tail, previousInstructions :+ currInstruction, (0,false))
      }
    }
  }

}
