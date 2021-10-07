package com.nosto.fun.game1.scala

import scala.util.Random

object Test {

  def main(args: Array[String]): Unit = {

    var a = Array.ofDim[Int](3, 3)
    a(0) = Array(1, 2, 3)
    a(1) = Array(1, 2, 3)
    a(2) = Array(5, 6, 10)
    val m = a.map(_.max).max
    println(m)

    val c = Random.nextInt(a(0).size)
    println(c)
  }


}
