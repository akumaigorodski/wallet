package immortan.utils


object Statistics {
  def removeExtremeOutliers[T](items: Seq[T] = Nil, lowerPct: Int = 10, upperPct: Int = 10)(extractor: T => Long): Seq[T] = items.size match { case size =>
    items.sortBy(extractor).drop(size / lowerPct).dropRight(size / upperPct)
  }

  def meanBy[T](items: Seq[T] = Nil)(extractor: T => Long): Double = items.foldLeft(0D) { case (total, item) => extractor(item).toDouble + total } / items.size

  def stdDevBy[T](items: Seq[T] = Nil, mean: Double)(extractor: T => Long): Double = {
    val variance = items.foldLeft(0D) { case (total, item) => math.pow(extractor(item) - mean, 2) + total } / items.size.toDouble
    math.sqrt(variance)
  }

  type IntegerTuple = (Int, Int)
  object InverseIntTupleComparator extends Ordering[IntegerTuple] {
    // Compare tuples by second elements, break ties by first elements
    override def compare(first: IntegerTuple, second: IntegerTuple): Int = {
      val result: Int = first._2.compareTo(second._2)
      if (result == 0) first._1.compareTo(second._1)
      else result
    }
  }
}