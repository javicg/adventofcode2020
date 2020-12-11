package com.github.javicg
package day11

import Utils.using

import scala.io.Source

object Solution extends App {
  val filename = "day11/input.txt"
  var seatMap = List.empty[String]
  using(Source.fromResource(filename)) { source =>
    for (line <- source.getLines()) {
      seatMap = seatMap :+ line
    }
  }

  val lounge = Lounge.from(seatMap)
  println(lounge.simulateFlow().countOccupiedSeats())
}
