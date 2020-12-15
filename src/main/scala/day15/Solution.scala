package com.github.javicg
package day15

import Utils.using

import scala.io.Source

object Solution extends App {
  val filename = "day15/input.txt"
  var input = Array.empty[Int]
  using(Source.fromResource(filename)) { source =>
    input = source.getLines().next().split(',').map(_.toInt)
  }

  var previousIdx = Map.empty[Int, Int]
  input.zipWithIndex.foreach(p => {
    println(s"input(${p._2}) = ${p._1}")
    previousIdx += (p._1 -> p._2)
  })

  println("...")

  val start = System.currentTimeMillis()

  var i = input.length
  var lastNumber = input.last
  while (i < 30000000) {
    val nextNumber = previousIdx.get(lastNumber)
      .map(idx => (i - 1) - idx)
      .getOrElse(0)

    previousIdx += (lastNumber -> (i - 1))
    lastNumber = nextNumber
    i += 1
  }

  val end = System.currentTimeMillis()

  println(s"input($i) = ${lastNumber}")
  println(s"Algorithm took ${end - start}ms")
}
