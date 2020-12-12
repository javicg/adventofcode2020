package com.github.javicg
package day12

import Utils.using

import scala.io.Source

object Solution extends App {
  val filename = "day12/input.txt"
  val logPattern = """(\w)([0-9]+)""".r
  var navigationLog = List.empty[Instruction]
  using(Source.fromResource(filename)) { source =>
    for (line <- source.getLines()) {
      val g = logPattern.findAllIn(line)
      val action = g.group(1).charAt(0)
      val arg = g.group(2).toInt
      navigationLog = navigationLog :+ Instruction(action, arg)
    }
  }

  val control = FerryControl()
  navigationLog.foreach(control.perform)
  println(control.getDistanceFromStart)
}
