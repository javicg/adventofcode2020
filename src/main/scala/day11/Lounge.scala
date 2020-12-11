package com.github.javicg

package day11

object Lounge {
  def from(seatMap: List[String]): Lounge = {
    var locations = List.empty[List[Location]]
    for ((row, i) <- seatMap.zipWithIndex) {
      var locationsInRow = List.empty[Location]
      for ((s, j) <- row.zipWithIndex) {
        locationsInRow = locationsInRow :+ Location(i,j,s)
      }
      locations = locations :+ locationsInRow
    }
    Lounge(locations)
  }
}

case class Lounge(var locations: List[List[Location]]) {
  def simulateFlow(): Unit = {
    val current = locations
    simulateNextRound()
    if (current.equals(locations)) {
      println(">> Simulation stable!")
      println(this)
    } else {
      simulateFlow()
    }
  }

  def countOccupiedSeats(): Int = {
    this.locations.flatten.count(l => l.isOccupied)
  }

  private def simulateNextRound(): Unit = {
    this.locations = locations.map(row => row.map(toNewState))
  }

  private def toNewState(location: Location): Location = {
    location match {
      case l if l.isEmpty && isComfortable(l) => Location(l.row, l.column, Location.Occupied)
      case l if l.isOccupied && isUncomfortable(l) => Location(l.row, l.column, Location.Empty)
      case l => l
    }
  }

  private def isComfortable(location: Location): Boolean = {
    getAdjacentLocations(location).count(_.isOccupied) == 0
  }

  private def isUncomfortable(location: Location): Boolean = {
    getAdjacentLocations(location).count(_.isOccupied) >= 5
  }

  private def getAdjacentLocations(location: Location): List[Location] = {
    List(
      getAdjacentLocation(location, -1, -1), getAdjacentLocation(location, -1, 0), getAdjacentLocation(location, -1, +1),
      getAdjacentLocation(location, 0, -1), /* Actual seat, then... */ getAdjacentLocation(location, 0, +1),
      getAdjacentLocation(location, +1, -1), getAdjacentLocation(location, +1, 0), getAdjacentLocation(location, +1, +1)
    ).filter(_.isDefined).map(_.get)
  }

  private def getAdjacentLocation(location: Location, rowMod: Int, seatMod: Int): Option[Location] = {
    try {
      val adjacentLocation = this.locations(location.row + rowMod)(location.column + seatMod)
      if (adjacentLocation.isFloor) {
        val shift: Int => Int = { i =>
          if (i < 0) {
            i - 1
          } else if (i == 0) {
            i
          } else {
            i + 1
          }
        }
        getAdjacentLocation(location, shift(rowMod), shift(seatMod))
      } else {
        Some(adjacentLocation)
      }
    } catch {
      case _: IndexOutOfBoundsException => None
    }
  }

  override def toString: String = {
    val sb = new StringBuilder
    sb.append("=" * locations.head.size + "\n")
    locations.map(row => row.mkString + "\n")
      .foreach(rowAsString => sb.append(rowAsString))
    sb.append("=" * locations.head.size + "\n")
    sb.toString
  }
}
