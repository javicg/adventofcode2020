package com.github.javicg
package day1

import Utils.using

import scala.io.Source

object Solution extends App {
  val filename = "day1/input.txt"
  var numbers: List[Int] = List()
  using(Source.fromResource(filename)) { source =>
    for (line <- source.getLines()) {
      numbers ::= line.toInt
    }
  }

  println(findSum(numbers, 2020))

  def findSum(numbers: List[Int], sum: Int): Int = {
    for (num1 <- numbers) {
      for (num2 <- numbers) {
        for (num3 <- numbers) {
          if (num1 + num2 + num3 == sum) {
            return num1 * num2 * num3
          }
        }
      }
    }
    -1
  }
}
