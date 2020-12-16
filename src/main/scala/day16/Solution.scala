package com.github.javicg
package day16

import Utils.using

import com.github.javicg.day16.Solution.mappedRules

import scala.io.Source

object Solution extends App {
  val ruleRegex = """^([\s\w]+): (\d+)-(\d+) or (\d+)-(\d+)$""".r

  val filename = "day16/input.txt"
  var myTicket: Ticket = _
  var nearbyTickets = List.empty[Ticket]
  var rules = List.empty[Rule]
  using(Source.fromResource(filename)) { source =>
    val lines = source.getLines()
    while (lines.hasNext) {
      val line = lines.next()
      if (ruleRegex.matches(line)) {
        val g = ruleRegex.findAllIn(line)
        rules = rules :+ Rule(
          g.group(1),
          Range.inclusive(g.group(2).toInt, g.group(3).toInt),
          Range.inclusive(g.group(4).toInt, g.group(5).toInt)
        )
      } else if (line.contains("your ticket:")) {
        myTicket = Ticket(lines.next().split(',').map(_.toInt))
      } else if (line.contains("nearby tickets:")) {
        for (line <- lines) {
          nearbyTickets = nearbyTickets :+ Ticket(line.split(',').map(_.toInt))
        }
      }
    }
  }

  val numInvalidFields = nearbyTickets.flatMap(getInvalidFields).sum
  println(numInvalidFields)

  val validTickets = nearbyTickets.filter(t => getInvalidFields(t).length == 0) :+ myTicket
  val mappedRules = mapRulesToValidPositions(rules, validTickets)
  val rulesWithPosition = deduplicateMappings(mappedRules)

  val departureValues = rules.filter(rule => rule.name.startsWith("departure"))
    .map(rule => rulesWithPosition(rule))
    .map(pos => myTicket.values(pos).toLong)
  println(departureValues.product)

  def getInvalidFields(ticket: Ticket): Array[Int] = {
    ticket.values.filter(v => rules.count(r => r.test(v)) == 0)
  }

  def mapRulesToValidPositions(rules: List[Rule], validTickets: List[Ticket]): Map[Rule, List[Int]] = {
    var mappedRules = Map.empty[Rule, List[Int]]
    for (rule <- rules) {
      validTickets.head.values.indices
        .filter(i => allValuesAtPositionAreValid(rule, validTickets, i))
        .foreach(i => mappedRules += rule -> (mappedRules.getOrElse(rule, List()) :+ i))
    }
    mappedRules
  }

  def allValuesAtPositionAreValid(rule: Rule, tickets: List[Ticket], idx: Int): Boolean = {
    tickets.map(_.values(idx)).count(v => rule.test(v)) == tickets.length
  }

  def deduplicateMappings(input: Map[Rule, List[Int]]): Map[Rule, Int] = {
    var identifiedRules = Map.empty[Rule, Int]
    var mapping = input
    while (mapping.values.exists(positions => positions.length > 1)) {
      val unidentifiedRule = mapping.find(entry => !identifiedRules.contains(entry._1) && entry._2.size == 1).map(_._1)
      if (unidentifiedRule.isDefined) {
        val correctPosition = mapping(unidentifiedRule.get).head
        mapping.filter(entry => !entry._1.equals(unidentifiedRule.get))
          .foreach(entry => mapping += entry._1 -> entry._2.filter(pos => pos != correctPosition))
        identifiedRules += (unidentifiedRule.get -> correctPosition)
      } else {
        throw new RuntimeException("Unable to continue identification. No single mapping found!")
      }
    }
    identifiedRules
  }

  case class Ticket(values: Array[Int])

  case class Rule(name: String, lower: Range, upper: Range) {
    def test(value: Int): Boolean = {
      lower.contains(value) || upper.contains(value)
    }
  }

}
