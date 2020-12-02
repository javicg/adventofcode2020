package com.github.javicg

import scala.io.Source

object Utils {

  def using[A](r: Source)(f: Source => A): A =
    try {
      f(r)
    } finally {
      r.close()
    }
}
