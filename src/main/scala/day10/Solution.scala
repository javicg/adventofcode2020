package com.github.javicg
package day10

import Utils.using

import scala.io.Source

object Solution extends App {
  type JoltageVariations = (Int, Int, Int)

  val filename = "day10/input.txt"
  var adapters = List.empty[Int]
  using(Source.fromResource(filename)) { source =>
    for (line <- source.getLines()) {
      adapters = adapters :+ line.toInt
    }
  }
  adapters = adapters :++ List(0, adapters.max + 3)
  adapters = adapters.sorted

  def calculateJoltageDifferences(adapters: List[Int]): JoltageVariations = {
    var variations: JoltageVariations = (0, 0, 0)
    var i = 0
    while (i < adapters.length - 1) {
      adapters(i + 1) - adapters(i) match {
        case 1 => variations = (variations._1 + 1, variations._2, variations._3)
        case 2 => variations = (variations._1, variations._2 + 1, variations._3)
        case 3 => variations = (variations._1, variations._2, variations._3 + 1)
      }
      i += 1
    }
    variations
  }

  var cachedCombinations = Map.empty[Int, Long]
  def calculatePossibleCombinations(adapters: List[Int]): Long = {
    if (adapters.length == 2) {
      val first = adapters.head
      val second = adapters(1)
      if (withinRange(first, second)) 1L else 0L
    } else {
      val first = adapters.head
      var acc = 0L
      val remainingAdapters = adapters.tail
      for ((adapter, idx) <- remainingAdapters.zipWithIndex) {
        if (withinRange(first, adapter)) {
          if (cachedCombinations.contains(adapter)) {
            acc += cachedCombinations(adapter)
          } else {
            val combinationsFromAdapter = calculatePossibleCombinations(remainingAdapters.slice(idx, remainingAdapters.length))
            cachedCombinations += adapter -> combinationsFromAdapter
            acc += combinationsFromAdapter
          }
        }
      }
      acc
    }
  }

  def withinRange(adapter1: Int, adapter2: Int): Boolean = {
    val diff = adapter2 - adapter1
    diff >= 1 && diff <= 3
  }

  val joltageVariations: (Int, Int, Int) = calculateJoltageDifferences(adapters)
  println(joltageVariations)
  println(joltageVariations._1 * joltageVariations._3)

  println("Calculating possible combinations...")
  println(s"Possible combinations with existing adapters: ${calculatePossibleCombinations(adapters)}")
}
