package com.github.javicg
package day9

import Utils.using

import scala.io.Source

object Solution extends App {
  val filename = "day9/input.txt"
  var numbers = List.empty[Long]
  using(Source.fromResource(filename)) { source =>
    for (line <- source.getLines()) {
      numbers = numbers :+ line.toLong
    }
  }

  def isValid(number: Long, preamble: List[Long]): Boolean = {
    for (a <- preamble) {
      for (b <- preamble) {
        if (a != b && a + b == number) {
          return true
        }
      }
    }
    false
  }

  def getFirstInvalidNumber(numbers: List[Long], preamble: Int): Option[Long] = {
    var i = 0
    var j = preamble
    while (j < numbers.length) {
      val number = numbers(j)
      if (!isValid(number, numbers.slice(i, j))) {
        return Some(number)
      }
      i += 1
      j += 1
    }
    None
  }

  def findValidationSet(numbers: List[Long], number: Long): Option[List[Long]] = {
    var i = 0
    while (i < numbers.length) {
      var j = i
      var acc = 0L
      while (j < numbers.length && acc < number) {
        acc += numbers(j)
        if (acc == number) {
          return Some(numbers.slice(i, j + 1))
        }
        j += 1
      }
      i += 1
    }
    None
  }

  val invalidNumber = getFirstInvalidNumber(numbers, 25).get
  println(s"Invalid number: $invalidNumber")

  val validationSet = findValidationSet(numbers, invalidNumber).get
  val min = validationSet.min
  val max = validationSet.max
  println(s"Min: $min | Max: $max | Sum: ${min + max}")
}
