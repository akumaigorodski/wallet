package com.github.vickumar1981.stringdistance.impl

private[stringdistance] trait DiceCoefficientImpl {
  protected def diceCoefficient[T](s1: Array[T], s2: Array[T]): Double = {
    val nx = s1.zip(s1.tail).toSet
    val ny = s2.zip(s2.tail).toSet
    val intersection = nx intersect ny
    2.0 * intersection.size / (nx.size + ny.size)
  }
}
