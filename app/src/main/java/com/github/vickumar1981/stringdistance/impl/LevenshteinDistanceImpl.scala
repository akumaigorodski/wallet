package com.github.vickumar1981.stringdistance.impl

import scala.math.min
import scala.language.postfixOps

private[stringdistance] trait LevenshteinDistanceImpl {
  protected def levenshtein[T](a: Array[T], b: Array[T]): Int =
    ((0 to b.length).toList /: a)((prev, x) =>
      (prev zip prev.tail zip b).scanLeft(prev.head + 1) {
        case (h, ((d, v), y)) => min(min(h + 1, v + 1), d + (if (x == y) 0 else 1))
      }) last

  protected def damerauLevenshtein[T](a: Array[T], b: Array[T]): Int = {
    val (aLength, bLength) = (a.length, b.length)

    (aLength, bLength) match {
      case (0, _) => bLength
      case (_, 0) => aLength
      case (_, _) => {
        val dist = Array.ofDim[Int](aLength + 1, bLength + 1)
        (0 to aLength).foreach { i => dist(i)(0) = i }
        (0 to bLength).foreach { j => dist(0)(j) = j }
        (1 to aLength).foreach { i =>
          {
            (1 to bLength).foreach { j =>
              {
                val cost = if (a(i - 1) == b(j - 1)) 0 else 1
                dist(i)(j) =
                  min(min(dist(i - 1)(j) + 1, dist(i)(j - 1) + 1), dist(i - 1)(j - 1) + cost)
                if (i > 1 && j > 1 && a(i - 1) == b(j - 2) && a(i - 2) == b(j - 1)) {
                  dist(i)(j) = min(dist(i)(j), dist(i - 2)(j - 2) + cost)
                }
              }
            }
          }
        }
        dist(aLength)(bLength)
      }
    }
  }
}
