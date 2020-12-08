package com.github.javicg
package day8

import Utils.using

import scala.io.Source

object Solution extends App {
  type Accumulator = Int
  type ProgramLine = Int

  val filename = "day8/input.txt"
  var program = List.empty[Instruction]
  using(Source.fromResource(filename)) { source =>
    val regex = """(\w+)\s([+-]\d+)""".r
    for (line <- source.getLines()) {
      val m = regex.findFirstMatchIn(line).get
      program = program :+ Instruction(m.group(1), m.group(2).toInt)
    }
  }

  def getLoopholeLineOrProgramEnd(program: List[Instruction]): (ProgramLine, Accumulator) = {
    var visitedLines = Set.empty[Int]
    var idx = 0
    var acc = 0
    while (idx < program.size && !visitedLines.contains(idx)) {
      visitedLines += idx
      program(idx) match {
        case Instruction("nop", _) =>
          idx += 1
        case Instruction("acc", qty) =>
          acc += qty
          idx += 1
        case Instruction("jmp", offset) =>
          idx += offset
      }
    }
    (idx, acc)
  }

  def getProgramStateAfterFix(program: List[Instruction]): (ProgramLine, Accumulator) = {
    for ((instruction, idx) <- program.zipWithIndex) {
      instruction match {
        case Instruction("jmp", offset) =>
          val state = getLoopholeLineOrProgramEnd(program.patch(idx, Seq(Instruction("nop", offset)), 1))
          if (state._1 == program.size) {
            return state
          }
        case Instruction("nop", qty) =>
          val state = getLoopholeLineOrProgramEnd(program.patch(idx, Seq(Instruction("jmp", qty)), 1))
          if (state._1 == program.size) {
            return state
          }
        case _ =>
      }
    }
    throw new RuntimeException("Unable to fix program :(")
  }

  println(getProgramStateAfterFix(program)._2)

  case class Instruction(code: String, arg: Int)

}
