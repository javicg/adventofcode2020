package com.github.javicg
package day5

import Utils.using

import scala.io.Source

object Solution extends App {
  val rows = Range.inclusive(0, 127).toList
  val columns = Range.inclusive(0, 7).toList

  val filename = "day5/input.txt"
  var highestSetId = -1

  def decodeSeatId(line: String): Int = {
    var row = rows
    var column = columns
    for (c <- line) {
      c match {
        case 'F' => row = row.slice(0, row.length / 2)
        case 'B' => row = row.slice(row.length / 2, row.length)
        case 'L' => column = column.slice(0, column.length / 2)
        case 'R' => column = column.slice(column.length / 2, column.length)
      }
    }
    if (row.length != 1 || column.length != 1) {
      throw new IllegalStateException(s"Row size [${row.length}] and Column size [${column.size}] are invalid!")
    }
    generateSeatId(row.head, column.head)
  }

  private def generateSeatId(row: Int, column: Int): Int = {
    row * 8 + column
  }

  var seats: Set[Int] = Set()
  using(Source.fromResource(filename)) { source =>
    for (line <- source.getLines()) {
      seats += decodeSeatId(line)
    }
  }

  for (r <- rows.slice(1, rows.length - 1)) {
    for (c <- columns) {
      val seatId = generateSeatId(r, c)
      if (!seats.contains(seatId) && seats.contains(seatId + 1) && seats.contains(seatId - 1)) {
        println(s"Found my seat! $seatId")
      }
    }
  }
}
