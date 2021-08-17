package immortan.utils


object Statistics {
  def removeExtremeOutliers[T, N](items: Seq[T] = Nil, lowerPct: Int = 10, upperPct: Int = 10)(extractor: T => N)(implicit n: Numeric[N]): Seq[T] = items.size match { case size =>
    items.sortBy(extractor).drop(size / lowerPct).dropRight(size / upperPct)
  }

  def meanBy[T, N](items: Seq[T] = Nil)(extractor: T => N)(implicit n: Numeric[N]): Double =
    items.foldLeft(0D) { case (total, item) => n.toDouble(extractor apply item) + total } / items.size

  def varianceBy[T, N](items: Seq[T] = Nil)(extractor: T => N)(implicit n: Numeric[N]): Double = meanBy(items)(extractor) match { case computedMean =>
    items.foldLeft(0D) { case (total, item) => math.pow(n.toDouble(extractor apply item) - computedMean, 2) + total } / items.size.toDouble
  }

  def stdDevBy[T, N](items: Seq[T] = Nil)(extractor: T => N)(implicit n: Numeric[N]): Double = {
    val computedVarianceBy = varianceBy(items)(extractor)
    math.sqrt(computedVarianceBy)
  }

  def zscoresBy[T, N](items: Seq[T] = Nil)(extractor: T => N)(implicit n: Numeric[N]): Seq[Double] = {
    val computedStdDevBy = stdDevBy(items)(extractor)
    val computedMeanBy = meanBy(items)(extractor)

    items.map { item =>
      val extractedValue = n.toDouble(extractor apply item)
      (extractedValue - computedMeanBy) / computedStdDevBy
    }
  }

  def pearsonBy[A, B, C](items: Seq[A] = Nil)(x: A => B)(y: A => C)(implicit n: Numeric[B], q: Numeric[C]): Double = {
    val computedZScores = zscoresBy(items)(x) zip zscoresBy(items)(y)

    computedZScores.foldLeft(0D) { case (total, items1) =>
      val (scoredXItem, scoredYItem) = items1
      scoredXItem * scoredYItem + total
    } / items.size.toDouble
  }
}