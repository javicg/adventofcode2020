package com.github.javicg
package day11

object Location {
  val Floor = '.'
  val Empty = 'L'
  val Occupied = '#'
}

case class Location(row: Int, column: Int, state:Char) {
  def isFloor: Boolean = state == Location.Floor
  def isEmpty: Boolean = state == Location.Empty
  def isOccupied: Boolean = state == Location.Occupied
  override def toString: String = state.toString
}
