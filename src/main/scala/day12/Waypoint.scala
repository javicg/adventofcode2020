package com.github.javicg

package day12

import day12.Direction._
import day12.Instruction.{Rotate, Shift}

object Waypoint {
  def apply(): Waypoint = new Waypoint(1, North, 10, East)
}

case class Waypoint(x: Int, xDir: Direction, y: Int, yDir: Direction) {
  def rotate(action: Rotate): Waypoint = {
    val xDirRotated = xDir.rotate(action)
    val yDirRotated = yDir.rotate(action)
    if (xDirRotated == East || xDirRotated == West) {
      Waypoint(y, yDirRotated, x, xDirRotated)
    } else {
      Waypoint(x, xDirRotated, y, yDirRotated)
    }
  }

  def shift(action: Shift): Waypoint = {
    action match {
      case Shift('N', arg) =>
        val newX = if (xDir == North) x + arg else x - arg
        Waypoint(newX, xDir, y, yDir)
      case Shift('S', arg) =>
        val newX = if (xDir == North) x - arg else x + arg
        Waypoint(newX, xDir, y, yDir)
      case Shift('E', arg) =>
        val newY = if (yDir == East) y + arg else y - arg
        Waypoint(x, xDir, newY, yDir)
      case Shift('W', arg) =>
        val newY = if (yDir == East) y - arg else y + arg
        Waypoint(x, xDir, newY, yDir)
    }
  }
}
