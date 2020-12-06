package com.github.javicg
package day6

import Utils.using

import scala.io.Source

object Solution extends App {
  val filename = "day6/input.txt"
  var acc = 0
  using(Source.fromResource(filename)) { source =>
    val lineIter: Iterator[String] = source.getLines()
    while (lineIter.hasNext) {
      var line = lineIter.next()
      var answers: Map[Char, Int] = Map()
      var groupSize = 0
      while (line.nonEmpty) {
        line.foreach(c => {
          answers += (c -> (answers.getOrElse(c, 0) + 1))
        })
        groupSize += 1
        if (lineIter.hasNext) {
          line = lineIter.next()
        } else {
          line = ""
        }
      }
      acc += answers.count(entry => entry._2 == groupSize)
    }
  }

  println(acc)
}
