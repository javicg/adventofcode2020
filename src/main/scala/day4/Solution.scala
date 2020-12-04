package com.github.javicg
package day4

import Utils.using

import scala.io.Source

object Solution extends App {
  case class Passport(fields: Map[String, String]) {
    def merge(passport: Passport): Passport = {
      Passport(fields ++ passport.fields)
    }

    def isValid: Boolean = {
      val containsNecessaryFields = fields.size == 8 || (fields.size == 7 && !fields.contains("cid"))
      val numValidFields = fields.count(data => {
        val value = data._2
        data._1 match {
          case "byr" =>
            1920 <= value.toInt && value.toInt <= 2002
          case "iyr" =>
            2010 <= value.toInt && value.toInt <= 2020
          case "eyr" =>
            2020 <= value.toInt && value.toInt <= 2030
          case "hgt" =>
            if (value.contains("cm")) {
              val height = value.substring(0, value.length - 2).toInt
              150 <= height && height <= 193
            } else if (value.contains("in")) {
              val height = value.substring(0, value.length - 2).toInt
              59 <= height && height <= 76
            } else {
              false
            }
          case "hcl" =>
            val pattern = """^#[0-9a-f]{6}$""".r
            pattern.matches(value)
          case "ecl" =>
            Set("amb", "blu", "brn", "gry", "grn", "hzl", "oth").contains(value)
          case "pid" =>
            val pattern = """^[0-9]{9}$""".r
            pattern.matches(value)
          case "cid" =>
            true
          case _ => false
        }
      })
      containsNecessaryFields && numValidFields == fields.size
    }
  }

  def passportFrom(line: String): Passport = {
    var values = Map[String, String]()
    for (pair <- line.split("\\s")) {
      val data = pair.split(':')
      values += (data(0) -> data(1))
    }
    Passport(values)
  }

  val filename = "day4/input.txt"

  var numValid = 0
  using(Source.fromResource(filename)) { source =>
    val lineIter: Iterator[String] = source.getLines()
    while (lineIter.hasNext) {
      var line = lineIter.next()
      var passport: Passport = Passport(Map())
      while (line.nonEmpty) {
        passport = passport.merge(passportFrom(line))
        if (lineIter.hasNext) {
          line = lineIter.next()
        } else {
          line = ""
        }
      }

      if (passport.isValid) {
        numValid += 1
      }
    }
  }

  println(numValid)
}
