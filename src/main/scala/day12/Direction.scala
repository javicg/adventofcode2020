package com.github.javicg
package day12

import day12.Direction.CoordMapping
import day12.Instruction.Rotate

sealed trait Direction {
  def rotate(action: Rotate): Direction = {
    action match {
      case Rotate('L', arg) =>
        val coord = (this.getInitialDegrees - arg) % 360
        CoordMapping(coord)
      case Rotate('R', arg) =>
        val coord = (this.getInitialDegrees + arg) % 360
        CoordMapping(coord)
    }
  }

  def getInitialDegrees: Int
}

object Direction {
  private val CoordMapping = Map(
    North.getInitialDegrees -> North,
    East.getInitialDegrees -> East,
    (East.getInitialDegrees - 360) -> East,
    South.getInitialDegrees -> South,
    (South.getInitialDegrees - 360) -> South,
    West.getInitialDegrees -> West,
    (West.getInitialDegrees - 360) -> West
  )

  object North extends Direction {
    override def getInitialDegrees: Int = 0
    override def toString: String = "N"
  }

  object South extends Direction {
    override def getInitialDegrees: Int = 180
    override def toString: String = "S"
  }

  object East extends Direction {
    override def getInitialDegrees: Int = 90
    override def toString: String = "E"
  }

  object West extends Direction {
    override def getInitialDegrees: Int = 270
    override def toString: String = "W"
  }
}
