package com.github.vickumar1981.stringdistance.impl

import com.github.vickumar1981.stringdistance.interfaces.CommonStringDistanceAlgo

private[stringdistance] trait JaroImpl extends CommonStringDistanceAlgo {
  protected def jaroWinkler[T](s1: Array[T], s2: Array[T], weight: Double = 0.1): Double = {
    require(weight >= 0 && weight <= 1, "Jaro-Winkler weight must be a number between 0 and 1.")
    val jaroScore = jaro(s1, s2)
    val l = minStartPrefix(s1, s2)
    jaroScore + (l * weight * (1 - jaroScore))
  }

  protected def jaro[T](s1: Array[T], s2: Array[T]): Double = {
    val minLen = math.min(s1.length, s2.length)
    val halflen = (minLen / 2) + (minLen % 2)
    val common1 = getCommonChars(s1.toList, s2.toList, halflen)
    val common2 = getCommonChars(s2.toList, s1.toList, halflen)
    if (common1.nonEmpty && common2.nonEmpty && common1.length == common2.length) {
      val transpositions = common1.indices
        .filterNot(idx => common1(idx) == common2(idx))
        .map { _ => 1d }
        .sum * 0.5d
      (((common1.length * 1d) / s1.length) +
        (common2.length * 1d / s2.length) +
        ((1d * common1.length - transpositions) / common1.length)) / 3d
    } else 0d
  }
}
