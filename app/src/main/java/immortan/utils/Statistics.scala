package immortan.utils


object Statistics {
  def removeExtremeOutliers[T](items: Seq[T] = Nil, lowerPct: Double, upperPct: Double)(extractor: T => Long): Seq[T] = items.size match { case size =>
    items.sortBy(extractor).drop(n = (size * lowerPct).toInt).dropRight(n = (size * upperPct).toInt)
  }

  def meanBy[T](items: Seq[T] = Nil)(extractor: T => Long): Double = {
    val mean = items.foldLeft(0D) { case (total, item) => extractor(item).toDouble + total }
    mean / items.size
  }

  def stdDevBy[T](items: Seq[T] = Nil, mean: Double)(extractor: T => Long): Double = {
    val variance = items.foldLeft(0D) { case (total, item) => math.pow(extractor(item) - mean, 2) + total }
    math.sqrt(variance / items.size.toDouble)
  }

  type LongIntTuple = (Int, Int)
  object InverseIntTupleComparator extends Ordering[LongIntTuple] {
    // Compare tuples by second elements, break ties by first elements
    override def compare(first: LongIntTuple, second: LongIntTuple): Int = {
      val result: Int = first._2.compareTo(second._2)
      if (result == 0) first._1.compareTo(second._1)
      else result
    }
  }
}