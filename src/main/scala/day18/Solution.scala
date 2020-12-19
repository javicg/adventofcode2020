package com.github.javicg
package day18

import Utils.using

import scala.io.Source

object Solution extends App {
  val filename = "day18/input.txt"

  val withEqualPrecedence = using(Source.fromResource(filename))(Math.parseEqualPrecedence)
  println(withEqualPrecedence.map(expr => expr.evaluate()).sum)

  val sumHigherPrecedence = using(Source.fromResource(filename))(Math.parseSumHigherPrecedence)
  println(sumHigherPrecedence.map(expr => expr.evaluate()).sum)

}
