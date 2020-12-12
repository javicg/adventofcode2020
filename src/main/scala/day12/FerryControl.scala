package com.github.javicg
package day12

import day12.Direction._
import day12.Instruction._

object FerryControl {
  def apply(): FerryControl = new FerryControl(0, 0, Waypoint())
}

case class FerryControl(var x: Int, var y: Int, var waypoint: Waypoint) {
  def perform(instruction: Instruction): Unit = {
    instruction match {
      case r: Rotate => waypoint = waypoint.rotate(r)
      case s: Shift => waypoint = waypoint.shift(s)
      case a: Advance => this.advance(a)
    }
  }

  def advance(action: Advance): Unit = {
    waypoint.xDir match {
      case North => x = x + waypoint.x * action.arg
      case South => x = x - waypoint.x * action.arg
    }
    waypoint.yDir match {
      case East => y = y + waypoint.y * action.arg
      case West => y = y - waypoint.y * action.arg
    }
  }

  def getDistanceFromStart: Int = {
    Math.abs(x) + Math.abs(y)
  }
}
