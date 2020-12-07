package com.github.javicg
package day7

import Utils.using

import scala.io.Source

object Solution extends App {
  val nonEmptyBagRegex = """([a-zA-Z]+( [a-zA-Z]+)*) bags contain""".r
  val innerBagsRegex = """((\d+) ([a-zA-Z]+( [a-zA-Z]+)*)) bags*""".r
  val emptyBagRegex = """([a-zA-Z]+( [a-zA-Z]+)*) bags contain no other bags\.""".r

  val filename = "day7/input.txt"
  var catalogue = Map.empty[String, Map[String, Int]]
  using(Source.fromResource(filename)) { source =>
    for (line <- source.getLines()) {
      if (emptyBagRegex.matches(line)) {
        val bag = emptyBagRegex.findAllIn(line).group(1)
        catalogue += (bag -> Map.empty[String, Int])
      } else {
        val bag = nonEmptyBagRegex.findAllIn(line).group(1)
        val innerBags = innerBagsRegex.findAllMatchIn(line)
        var innerBagCount = Map.empty[String, Int]
        for (innerBag <- innerBags) {
          innerBagCount += (innerBag.group(3) -> innerBag.group(2).toInt)
        }
        catalogue += (bag -> innerBagCount)
      }
    }
  }

  println(countInnerBags("shiny gold"))

  def countContainersFor(queryBag: String): Int = {
    getUniquePossibleContainers(queryBag).size
  }

  def getUniquePossibleContainers(queryBag: String): Set[String] = {
    var containers = Set.empty[String]
    catalogue.foreachEntry((containerBag, details) => {
      if (details.contains(queryBag)) {
        containers += containerBag
        containers ++= getUniquePossibleContainers(containerBag)
      }
    })
    containers
  }

  def countInnerBags(queryBag: String): Long = {
    var numInnerBags = 0L
    catalogue(queryBag).foreachEntry((innerBag, count) => {
      numInnerBags += count
      numInnerBags += count * countInnerBags(innerBag)
    })
    numInnerBags
  }
}
