package com.github.javicg
package day18

import scala.util.matching.Regex

object Expression {
  val EndsWithNumber: Regex = """(\d+)$""".r
  val SumLike: Regex = """^(.*) \+$""".r
  val ProdLike: Regex = """^(.*) \*$""".r

  case class Literal(value: Long) extends Expression {
    override def evaluate(): Long = value
  }
  case class Sum(left: Expression, right: Expression) extends Expression {
    override def evaluate(): Long = left.evaluate() + right.evaluate()
  }
  case class Product(left: Expression, right: Expression) extends Expression {
    override def evaluate(): Long = left.evaluate() * right.evaluate()
  }

  def parse(line: String): Expression = {
    var trimmedLine = line.trim
    var rightExpr: Expression = null
    if (trimmedLine.last == ')') {
      val (expr, i) = extractSubExpression(trimmedLine.substring(0, trimmedLine.length - 1))
      rightExpr = expr
      trimmedLine = trimmedLine.substring(0, i)
    } else {
      val number = EndsWithNumber.findFirstMatchIn(trimmedLine).get.group(1)
      rightExpr = Literal(number.toLong)
      trimmedLine = trimmedLine.substring(0, trimmedLine.length - number.length)
    }

    trimmedLine = trimmedLine.trim
    if (SumLike.matches(trimmedLine)) {
      val g = SumLike.findFirstMatchIn(trimmedLine)
      val leftExpr = g.get.group(1)
      Sum(parse(leftExpr), rightExpr)
    } else if (ProdLike.matches(trimmedLine)) {
      val g = ProdLike.findFirstMatchIn(trimmedLine)
      val leftExpr = g.get.group(1)
      Product(parse(leftExpr), rightExpr)
    } else {
      rightExpr
    }
  }

  private def extractSubExpression(line: String): (Expression, Int) = {
    var counter = 0
    for (i <- (line.length - 1) to 0 by -1) {
      if (line.charAt(i).equals('(') && counter == 0) {
        return (parse(line.substring(i + 1, line.length)), i)
      } else if (line.charAt(i).equals('(') && counter != 0) {
        counter -= 1
      } else if (line.charAt(i) == ')') {
        counter += 1
      }
    }
    throw new RuntimeException(s"Unbalanced parenthesis on expression: $line")
  }
}

sealed trait Expression {
  def evaluate(): Long
}
