package com.github.javicg
package day12

sealed trait Instruction {
  def action: Char
  def arg: Int
}

object Instruction {
  def apply(action: Char, arg: Int): Instruction = {
    action match {
      case 'R' | 'L' => Rotate(action, arg)
      case 'N' | 'S' | 'E' | 'W' => Shift(action, arg)
      case 'F' => Advance(action, arg)
    }
  }
  case class Rotate(action: Char, arg: Int) extends Instruction
  case class Shift(action: Char, arg: Int) extends Instruction
  case class Advance(action: Char, arg: Int) extends Instruction
}
