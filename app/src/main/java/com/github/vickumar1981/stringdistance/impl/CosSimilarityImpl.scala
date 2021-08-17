package com.github.vickumar1981.stringdistance.impl

private[stringdistance] trait CosSimilarityImpl {
  private def termFrequencyMap[T](terms: Seq[T]): Map[T, Int] = {
    val retMap = scala.collection.mutable.Map[T, Int]()
    terms.foreach { t =>
      {
        val n = retMap.getOrElse(t, 0)
        retMap += ((t, n + 1))
      }
    }
    retMap.toMap
  }

  protected def cosSimilarity[T](s1: Array[T], s2: Array[T]): Double = {
    val (s1TermFreqs, s2TermFreqs) = (termFrequencyMap(s1.toSeq), termFrequencyMap(s2.toSeq))
    val intersection = (s1TermFreqs.keySet intersect s2TermFreqs.keySet).toList
    val dotProduct = intersection.map { i => s1TermFreqs(i) * s2TermFreqs(i) }.sum
    val magnitudeS1 = s1TermFreqs.values.map { i => i * i }.sum
    val magnitudeS2 = s2TermFreqs.values.map { i => i * i }.sum
    dotProduct / math.sqrt(magnitudeS1 * magnitudeS2)
  }
}
