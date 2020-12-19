package com.github.javicg
package day19

import Utils.using

import scala.io.Source

object Solution extends App {
  val ruleRegex = """^(\d+):(.*)$""".r
  val filename = "day19/input.txt"

  var rawRules = Map.empty[Int, String]
  var messages = List.empty[String]
  using(Source.fromResource(filename)) { source =>
    val lines = source.getLines()
    for (line <- lines) {
      if (ruleRegex.matches(line)) {
        val g = ruleRegex.findFirstMatchIn(line).get
        rawRules = rawRules + (g.group(1).toInt -> g.group(2).trim)
      } else {
        for (line <- lines) {
          messages = messages :+ line
        }
      }
    }
  }

  val unfoldedRules = unfoldRules(rawRules)
  val rule0 = unfoldedRules(0)
  println(messages.count(msg => rule0.isValid(msg)))

  val alternativeRule0 = AlternativeRule0(unfoldedRules(42), unfoldedRules(31))
  println(messages.count(msg => alternativeRule0.isValid(msg)))

  def unfoldRules(rawRules: Map[Int, String]): Map[Int, Rule] = {
    val fixedInputRegex = """^"(\w+)"$""".r
    var unfoldedRules = Map.empty[Int, Rule]
    while (unfoldedRules.size != rawRules.size) {
      rawRules.foreachEntry((idx, description) => {
        if (fixedInputRegex.matches(description)) {
          val g = fixedInputRegex.findFirstMatchIn(description).get
          unfoldedRules = unfoldedRules + (idx -> Rule(Set(g.group(1))))
        } else {
          val options = description.split('|').map(_.trim)
          if (options.forall(opt => canBeUnfolded(opt, unfoldedRules))) {
            unfoldedRules = unfoldedRules + (idx -> Rule(constructOptions(options, unfoldedRules)))
          }
        }
      })
    }

    unfoldedRules
  }

  private def canBeUnfolded(opt: String, unfoldedRules: Map[Int, Rule]): Boolean = {
    opt.split("\\s").forall(c => unfoldedRules.contains(c.toInt))
  }

  def constructOptions(options: Array[String], unfoldedRules: Map[Int, Rule]): Set[String] = {
    options.flatMap(opt => {
      var validation = Set.empty[String]
      for (c <- opt.split("\\s")) {
        val rule = unfoldedRules(c.toInt)
        if (validation.isEmpty) {
          validation = rule.validation
        } else {
          validation = validation.flatMap(v1 => rule.validation.map(v2 => v1 + v2))
        }
      }
      validation
    }).toSet
  }

  case class Rule(validation: Set[String]) {
    def isValid(input: String): Boolean = validation.contains(input)
  }

  case class AlternativeRule0(prefix: Rule, suffix: Rule) {
    private val PrefixLength = prefix.validation.head.length
    private val SuffixLength = suffix.validation.head.length

    def isValid(input: String): Boolean = {
      var numMatchingPrefix = 0
      var numMatchingSuffix = 0
      var substr = input

      var matchingPrefix = true
      while (substr.length >= PrefixLength && matchingPrefix) {
        val currentSubstr = substr.substring(0, PrefixLength)
        if (prefix.isValid(currentSubstr)) {
          numMatchingPrefix += 1
          substr = substr.substring(PrefixLength, substr.length)
        } else {
          matchingPrefix = false
        }
      }

      var matchingSuffix = true
      while (substr.length >= SuffixLength && matchingSuffix) {
        val currentSubstr = substr.substring(0, SuffixLength)
        if (suffix.isValid(currentSubstr)) {
          numMatchingSuffix += 1
          substr = substr.substring(SuffixLength, substr.length)
        } else {
          matchingSuffix = false
        }
      }

      numMatchingPrefix >= 2 && numMatchingSuffix >= 1 && numMatchingPrefix > numMatchingSuffix && substr.isEmpty
    }
  }

}
