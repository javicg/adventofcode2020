package com.github.javicg
package day17

import Utils.using

import scala.io.Source

object Solution extends App {
  val Active = '#'
  val Inactive = '.'

  val filename = "day17/input.txt"
  var activeCubes = Set.empty[Cube]
  using(Source.fromResource(filename)) { source =>
    for ((line, y) <- source.getLines().zipWithIndex) {
      for ((state, x) <- line.zipWithIndex) {
        if (state == Active) {
          activeCubes = activeCubes + Cube(x, y, 0, 0)
        }
      }
    }
  }

  val numCycles = 6
  Range(0, numCycles).foreach(_ => activeCubes = simulateCycle(activeCubes))
  println(activeCubes.size)

  def simulateCycle(activeCubes: Set[Cube]): Set[Cube] = {
    activeCubes.flatMap(cube => {
      val neighboursAndSelf = getNeighbours(cube) :+ cube
      neighboursAndSelf.filter(cube => shouldActivate(cube, activeCubes))
    })
  }

  def shouldActivate(cube: Cube, activeCubes: Set[Cube]): Boolean = {
    val activeNeighbours = getNeighbours(cube).filter(cube => activeCubes.contains(cube))
    if (activeCubes.contains(cube))
      activeNeighbours.size == 2 || activeNeighbours.size == 3
    else
      activeNeighbours.size == 3
  }

  def getNeighbours(cube: Cube): List[Cube] = {
    for (
      x <- List(-1, 0, 1);
      y <- List(-1, 0, 1);
      z <- List(-1, 0, 1);
      w <- List(-1, 0, 1)
      if (x, y, z, w) != (0, 0, 0, 0)
    ) yield Cube(cube.x + x, cube.y + y, cube.z + z, cube.w + w)
  }

  case class Cube(x: Int, y: Int, z: Int, w: Int)

}
