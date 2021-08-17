package com.github.vickumar1981.stringdistance.impl

import scala.math.{max, min}

private[stringdistance] trait SmithWatermanImpl extends GapSubstitution {
  def smithWaterman[T](
      s1: Array[T],
      s2: Array[T],
      gap: Gap = LinearGap(),
      windowSize: Int = Integer.MAX_VALUE): Double = {
    require(gap.matchValue > 0, "Smith Waterman match value must be a number > 0.")
    require(gap.misMatchValue < 0, "Smith Waterman mismatch value must be a number < 0.")
    require(gap.gapValue <= 0, "Smith Waterman gap value must be a number <= 0")
    require(windowSize > 0, "Smith Waterman window size must be a number > 0")

    if (s1.isEmpty || s2.isEmpty) 0d
    else {
      val maxDist = min(s1.length, s2.length) * max(gap.matchValue, gap.min)
      val calcScore = calculateSmithWaterman(s1, s2, gap, windowSize)
      calcScore / maxDist
    }
  }

  def smithWatermanGotoh[T](
      s1: Array[T],
      s2: Array[T],
      gap: ConstantGap = ConstantGap()): Double = {
    require(gap.matchValue > 0, "Smith Waterman Gotoh match value must be a number > 0.")
    require(gap.misMatchValue < 0, "Smith Waterman Gotoh mismatch value must be a number < 0.")
    require(gap.gapValue <= 0, "Smith Waterman Gotoh gap value must be a number <= 0")
    if (s1.isEmpty || s2.isEmpty) 0d
    else {
      val maxDist = min(s1.length, s2.length) * max(gap.matchValue, gap.gapValue)
      val calcScore = calculateSmithWatermanGotoh(s1, s2, gap)
      calcScore / maxDist
    }
  }

  // scalastyle:off
  private def calculateSmithWaterman[T](
      s1: Array[T],
      s2: Array[T],
      gap: Gap,
      windowSize: Int): Double = {
    val (s1Len, s2Len) = (s1.length, s2.length)
    val d = Array.ofDim[Double](s1Len, s2Len)
    var maxValue: Double = max(0d, subst(s1, 0, s2, 0, gap))
    d(0)(0) = maxValue
    s1.indices.foreach { i =>
      {
        // Get the optimal deletion
        var maxGapCost = 0d
        (max(1, i - windowSize) until i).foreach { k =>
          maxGapCost = max(maxGapCost, d(i - k)(0) + gap.value(i - k, i))
        }
        d(i)(0) = max(max(0, maxGapCost), subst(s1, i, s2, 0, gap))
        maxValue = max(maxValue, d(i)(0))
      }
    }

    (1 until s2Len).foreach { j =>
      {
        // Get the optimal insertion
        var maxGapCost = 0d
        (max(1, j - windowSize) until j).foreach { k =>
          maxGapCost = max(maxGapCost, d(0)(j - k) + gap.value(j - k, j))
        }
        d(0)(j) = max(max(0d, maxGapCost), subst(s1, 0, s2, j, gap))
        maxValue = max(maxValue, d(0)(j))
      }
    }

    // Build 2-d array
    (1 until s1Len).foreach { i =>
      {
        (1 until s2Len).foreach { j =>
          {
            var maxGapCost = 0d
            (max(1, i - windowSize) until i).foreach { k =>
              maxGapCost = max(maxGapCost, d(i - k)(j) + gap.value(i - k, i))
            }

            (max(1, j - windowSize) until j).foreach { k =>
              maxGapCost = max(maxGapCost, d(i)(j - k) + gap.value(j - k, j))
            }

            d(i)(j) = max(max(0d, maxGapCost), d(i - 1)(j - 1) + subst(s1, i, s2, j, gap))
            maxValue = max(maxValue, d(i)(j))
          }
        }
      }
    }
    maxValue
  }

  // scalastyle: on

  private def calculateSmithWatermanGotoh[T](
      s1: Array[T],
      s2: Array[T],
      gap: ConstantGap): Double = {
    val (s1Len, s2Len) = (s1.length, s2.length)
    val v0 = Array.ofDim[Double](s2Len)
    val v1 = Array.ofDim[Double](s2Len)
    var maxValue = max(max(0, gap.gapValue), subst(s1, 0, s2, 0, gap))

    (1 until s2Len).foreach { j =>
      v0(j) = max(max(0, v0(j - 1) + gap.gapValue), subst(s1, 0, s2, j, gap))
    }

    (1 until s1Len).foreach { i =>
      {
        v1(0) = max(max(0, v0(0) + gap.gapValue), subst(s1, i, s2, 0, gap))
        maxValue = max(maxValue, v1(0))

        (1 until s2Len).foreach { j =>
          {
            v1(j) = max(max(0, v0(j) + gap.gapValue), v0(j - 1) + subst(s1, i, s2, j, gap))
            maxValue = max(maxValue, v1(j))
          }
        }

        s2.indices.foreach { j =>
          v0(j) = v1(j)
        }
      }
    }
    maxValue
  }
}
