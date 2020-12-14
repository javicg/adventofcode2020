package com.github.javicg
package day14

import Utils.using

import scala.io.Source

object Solution extends App {
  val maskRegex = """mask = (\w+)""".r
  val assignRegex = """mem\[(\d+)] = (\d+)""".r
  val filename = "day14/input.txt"
  var instructions = List.empty[Instruction]
  using(Source.fromResource(filename)) { source =>
    for (line <- source.getLines()) {
      if (maskRegex.matches(line)) {
        val g = maskRegex.findAllIn(line)
        instructions = instructions :+ Bitmask(g.group(1))
      } else {
        val g = assignRegex.findAllIn(line)
        instructions = instructions :+ Assignment(g.group(1).toInt, g.group(2).toLong)
      }
    }
  }

  var memoryMap = Map.empty[Long, Long]
  var currentBitmask: Bitmask = _
  for (instr <- instructions) {
    instr match {
      case bitmask: Bitmask =>
        currentBitmask = bitmask
      case assignment: Assignment =>
        val maskedAddr = currentBitmask.maskV2(assignment.memaddr)
        Bitmask.unfold(maskedAddr).foreach(addr => memoryMap += (addr -> assignment.value))
    }
  }

  println(s"Sum of memory values: ${memoryMap.values.sum}")

  sealed trait Instruction

  object Bitmask {
    def unfold(maskedAddr: String): Set[Long] = {
      var addresses = Set("")
      for (bit <- maskedAddr) {
        bit match {
          case 'X' =>
            addresses = addresses.flatMap(partialAddress => Set(partialAddress + '0', partialAddress + '1'))
          case _ =>
            addresses = addresses.map(partialAddress => partialAddress :+ bit)
        }
      }
      addresses.map(addr => fromBinary(addr))
    }

    def toBinary(value: Long): String = {
      val binaryStr = java.lang.Long.toString(value, 2)
      "0" * (36 - binaryStr.length) + binaryStr
    }

    def fromBinary(value: String): Long = {
      java.lang.Long.valueOf(value, 2)
    }
  }

  case class Bitmask(mask: String) extends Instruction {
    def maskV1(value: Long): Long = {
      val maskedValue = Bitmask.toBinary(value)
        .zip(mask)
        .map(pair =>
          pair._2 match {
            case 'X' => pair._1
            case _ => pair._2
          })
        .mkString
      Bitmask.fromBinary(maskedValue)
    }

    def maskV2(value: Long): String = {
      Bitmask.toBinary(value)
        .zip(mask)
        .map(pair =>
          pair._2 match {
            case '0' => pair._1
            case '1' => '1'
            case 'X' => 'X'
          })
        .mkString
    }
  }

  case class Assignment(memaddr: Long, value: Long) extends Instruction

}
