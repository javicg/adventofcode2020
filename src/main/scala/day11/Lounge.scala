package com.github.javicg

package day11

object Lounge {
  def from(seatMap: List[String]): Lounge = {
    val locations: List[List[Location]] = seatMap.zipWithIndex.map(r =>
      r._1.zipWithIndex.map(s =>
        Location(r._2, s._2, s._1)
      ).toList
    )
    Lounge(locations)
  }
}

case class Lounge(locations: List[List[Location]]) {
  def simulateFlow(): Lounge = {
    val newLounge = simulateNextRound()
    if (this.equals(newLounge)) this else newLounge.simulateFlow()
  }

  def countOccupiedSeats(): Int = {
    this.locations.flatten.count(l => l.isOccupied)
  }

  private def simulateNextRound(): Lounge = {
    Lounge(locations.map(row => row.map(toNewState)))
  }

  private def toNewState(location: Location): Location = {
    location match {
      case l if l.isEmpty && isComfortable(l) => l.occupy
      case l if l.isOccupied && isUncomfortable(l) => l.leave
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
}
