package com.github.javicg
package day2

import Utils.using

import scala.io.Source

object Solution extends App {
  case class Policy(min: Int, max: Int, letter: Char)
  case class Entry(password: String, policy: Policy)

  val filename = "day2/input.txt"

  var entries: List[Entry] = List()
  using(Source.fromResource(filename)) { source =>
    val pattern = """^(\d+)-(\d+)\s(.):\s(\w+)$""".r
    for (line <- source.getLines()) {
      line match {
        case pattern(min, max, letter, password) => entries ::= Entry(password, Policy(min.toInt, max.toInt, letter.charAt(0)))
      }
    }
  }

  def isValid_oldPolicy(entry: Entry): Boolean = {
    val count = entry.password.count(c => c.equals(entry.policy.letter))
    count >= entry.policy.min && count <= entry.policy.max
  }

  def isValid_newPolicy(entry: Entry): Boolean = {
    val firstChar = entry.password.charAt(entry.policy.min - 1)
    val secondChar = entry.password.charAt(entry.policy.max - 1)
    val firstCharEquals = firstChar == entry.policy.letter
    val secondCharEquals = secondChar == entry.policy.letter
    (firstCharEquals && !secondCharEquals) || (!firstCharEquals && secondCharEquals)
  }

  println(entries.count(isValid_newPolicy))
}
