package com.github.vickumar1981.stringdistance.impl

private[stringdistance] trait GapSubstitution {
  protected def subst[T](a: Array[T], aIndex: Int, b: Array[T], bIndex: Int, gap: Gap): Double = {
    if (a(aIndex) == b(bIndex)) gap.matchValue else gap.misMatchValue
  }
}

trait Gap {
  def matchValue: Double
  def misMatchValue: Double
  def gapValue: Double
  def value(fromIndex: Double, toIndex: Double): Double
  def max: Double
  def min: Double
}

case class ConstantGap(matchValue: Double = 1, misMatchValue: Double = -1, gapValue: Double = 0)
    extends Gap {
  def value(fromIndex: Double, toIndex: Double): Double = gapValue
  def value: Double = gapValue
  def max: Double = gapValue
  def min: Double = gapValue
}

case class LinearGap(matchValue: Double = 1, misMatchValue: Double = -1, gapValue: Double = -1)
    extends Gap {
  def value(fromIndex: Double, toIndex: Double): Double = gapValue * (toIndex - fromIndex - 1)
  def max: Double = 0
  def min: Double = Double.NegativeInfinity
}

case class AffineGap(
    matchValue: Double = 1,
    misMatchValue: Double = -1,
    startValue: Double = 0,
    gapValue: Double = -1)
    extends Gap {
  def value(fromIndex: Double, toIndex: Double): Double =
    startValue + gapValue * (toIndex - fromIndex - 1)
  def max: Double = startValue
  def min: Double = Double.NegativeInfinity
}
