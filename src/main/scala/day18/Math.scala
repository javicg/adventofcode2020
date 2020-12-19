package com.github.javicg
package day18

import scala.io.Source

object Math {
  def parseEqualPrecedence(source: Source): List[Expression] = {
    source.getLines().map(Expression.parse).toList
  }

  def parseSumHigherPrecedence(source: Source): List[Expression] = {
    source.getLines().map(fixPrecedence).map(Expression.parse).toList
  }

  def fixPrecedence(line: String): String = {
    val idx = line.lastIndexOf('+')
    if (idx < 0) {
      line.replaceAll("SUM", "+")
    } else {
      val leftIdx = if (line.charAt(idx - 2) != ')') idx - 2 else findOutermostOpenParenthesis(line.substring(0, idx - 1))
      val rightIdx = if (line.charAt(idx + 2) != '(') idx + 2 else idx + 2 + findOutermostClosedParenthesis(line.substring(idx + 2, line.length))
      val processedLine = line.substring(0, leftIdx) +
        '(' + line.substring(leftIdx, idx) +
        line.substring(idx, idx + 1).replace("+", "SUM") +
        line.substring(idx + 1, rightIdx + 1) + ')' +
        line.substring(rightIdx + 1, line.length)
      fixPrecedence(processedLine)
    }
  }

  private def findOutermostOpenParenthesis(substr: String): Int = {
    var counter = 0
    for (i <- (substr.length - 2) to 0 by -1) {
      if (substr.charAt(i).equals('(') && counter == 0) {
        return i
      } else if (substr.charAt(i).equals('(') && counter != 0) {
        counter -= 1
      } else if (substr.charAt(i) == ')') {
        counter += 1
      }
    }
    throw new RuntimeException(s"Unbalanced parenthesis on expression: $substr)")
  }

  private def findOutermostClosedParenthesis(substr: String): Int = {
    var counter = 0
    for (i <- 1 until substr.length) {
      if (substr.charAt(i).equals(')') && counter == 0) {
        return i
      } else if (substr.charAt(i).equals(')') && counter != 0) {
        counter -= 1
      } else if (substr.charAt(i) == '(') {
        counter += 1
      }
    }
    throw new RuntimeException(s"Unbalanced parenthesis on expression: ($substr")
  }
}
