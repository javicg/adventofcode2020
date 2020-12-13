package com.github.javicg
package day13

import Utils.using

import scala.io.Source

object Solution extends App {
  val filename = "day13/input.txt"
  var estimateForArrival = -1
  var buses = List.empty[Bus]
  using(Source.fromResource(filename)) { source =>
    val lines = source.getLines()
    estimateForArrival = lines.next().toInt
    buses = lines.next().split(',').zipWithIndex
      .filterNot(_._1.equals("x"))
      .map(busWithIndex => Bus(busWithIndex._1.toInt, busWithIndex._2)).toList
  }

  val result = findEarliestBusAndWaitTime(estimateForArrival, buses)
  println(result._1.number * result._2)

  println(s"Earliest timestamp: ${getEarliestTimestamp(buses)}")

  def findEarliestBusAndWaitTime(arrivalTime: Int, buses: List[Bus]): (Bus, Int) = {
    val nearestDepartureTimePerBus = buses.map(bus => arrivalTime - arrivalTime % bus.number)
    val exactDepartureTime = nearestDepartureTimePerBus.zipWithIndex.find(pair => pair._1 == arrivalTime)
    if (exactDepartureTime.isDefined) {
      (buses(exactDepartureTime.get._2), 0)
    } else {
      val nextDepartureTimePerBus = nearestDepartureTimePerBus.zipWithIndex.map(pair => pair._1 + buses(pair._2).number)
      val earliestDeparture = nextDepartureTimePerBus.min
      val waitTime = earliestDeparture - arrivalTime
      val targetBus = buses(nextDepartureTimePerBus.indexOf(earliestDeparture))
      (targetBus, waitTime)
    }
  }

  def getEarliestTimestamp(buses: List[Bus]): Long = {
    var timestamp = 1L
    var incr = 1L
    for (bus <- buses) {
      while ((timestamp + bus.lane) % bus.number != 0) {
        timestamp += incr
      }
      incr *= bus.number
    }
    timestamp
  }

  case class Bus(number: Int, lane: Int)

}
