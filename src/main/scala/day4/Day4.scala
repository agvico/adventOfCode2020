package day4

import scala.io.Source

object Day4 {

  def main(args: Array[String]): Unit = {

    val data = Source.fromFile("src/main/scala/day4/input").mkString
    val regex = "\n\n".r
    val validStates = Array("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid") //,"cid")


    // part1
    val passports = regex.split(data).map(l => getPassport(l))
    val validPassports = passports.filter(p => isValid(validStates, p))
    println(validPassports.length)

    // part2
    val validAndPresent = validPassports.count(_.find(v =>  !checksRules(v._1,v._2))
      match {
        case Some(value) => false    // If some, an error is found
        case None => true           // if none, no errors found
      })

    println(validAndPresent)
  }

  /**
   * It returns a map with key-value pairs representing a passport
   * @param l
   * @return
   */
  def getPassport(l: String): Map[String, String] = {
    l.split("\\s").map(i => {
      val el = i.split(":")
      (el(0) -> el(1))
    }).toMap
  }

  /**
   * checks if the given passport contains all the valid elements.
   * @param validStates
   * @param passport
   * @return
   */
  def isValid(validStates: Array[String], passport: Map[String, String]): Boolean = {
    validStates.map(v => passport.get(v)).find { // if a valid value is present, return Some, else None
      case Some(value) => false
      case None => true
    } match {
      case Some(value) => false
      case None => true
    }
  }


  /**
   * Check is an entry of a passport is valid.
   * @param key
   * @param value
   * @return
   */
  def checksRules(key: String, value: String): Boolean ={
    key match {
      case "byr" => if( "\\d{4}".r.matches(value) ) value.toInt >= 1920 && value.toInt <= 2002 else false
      case "iyr" => if( "\\d{4}".r.matches(value) ) value.toInt >= 2010 && value.toInt <= 2020 else false
      case "eyr" => if( "\\d{4}".r.matches(value) ) value.toInt >= 2020 && value.toInt <= 2030 else false
      case "hgt" => {
        if ("\\d+(cm|in)".r.matches(value)) {
          val number = value.substring(0, value.length - 2)
          if ("\\d+cm".r.matches(value)) {
            number.toInt >= 150 && number.toInt <= 193
          } else {
            number.toInt >= 59 && number.toInt <= 76
          }
        } else {
          false
        }
      }
      case "hcl" => "#[0-9a-f]{6}".r.matches(value)
      case "ecl" => "amb|blu|brn|gry|grn|hzl|oth".r.matches(value)
      case "pid" => "\\d{9}".r.matches(value)
      case _ => true
    }
  }

}
