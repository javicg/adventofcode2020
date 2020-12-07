package com.github.javicg
package day3

import Utils.using

import scala.io.Source

object Solution extends App {
  type GeoMap = List[String]
  val Tree = '#'
  val Empty = '.'

  val filename = "day3/input.txt"
  var map: GeoMap = List()
  using(Source.fromResource(filename)) { source =>
    for (line <- source.getLines()) {
      map = map :+ line
    }
  }

  println(
    countTrees(map, 1, 1)
      * countTrees(map, 3, 1)
      * countTrees(map, 5, 1)
      * countTrees(map, 7, 1)
      * countTrees(map, 1, 2)
  )

  def countTrees(map: GeoMap, rightSlope: Int, downSlope: Int): Long = {
    var pos = (0, 0)
    var counter = 0L
    while (pos._1 < map.length - 1) {
      pos = (pos._1 + downSlope, (pos._2 + rightSlope) % map.head.length)
      if (map(pos._1)(pos._2) == Tree) {
        counter += 1
      }
    }
    counter
  }
}
