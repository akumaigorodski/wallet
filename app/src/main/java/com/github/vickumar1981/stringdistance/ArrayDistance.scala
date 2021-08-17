package com.github.vickumar1981.stringdistance

import com.github.vickumar1981.stringdistance.impl._

/**
 * Main class to work with generic arrays, Array[T], analagous to [[StringDistance]]
 *
 * {{{
 * import com.github.vickumar1981.stringdistance.ArrayDistance._
 *
 * // Example Levenshtein Distance and Score
 * val levenshteinDist = Levenshtein.distance(Array("m", "a", "r", "t", "h", "a"), Array("m", "a", "r", "h", "t", "a")) // 2
 * val levenshtein = Levenshtein.score(Array("m", "a", "r", "t", "h", "a"), Array("m", "a", "r", "h", "t", "a")) // 0.667
 * }}}
 */

object ArrayDistance {
  protected def wrapDist[T](arr1: Array[T], arr2: Array[T], dist: () => Int) =
    if (arr1.isEmpty && arr2.isEmpty) 0 else dist()
  protected def wrapScore[T](arr1: Array[T], arr2: Array[T], score: () => Double) =
    if (arr1.isEmpty && arr2.isEmpty) 1d else score()

  sealed trait ScoreMetric {
    def score[T](arr1: Array[T], arr2: Array[T]): Double
  }

  sealed trait DistanceMetric {
    def distance[T](arr1: Array[T], arr2: Array[T]): Int
  }

  sealed trait WeightedScoreMetric[T2] {
    def score[T1](arr1: Array[T1], arr2: Array[T1], weight: T2): Double
    def score[T1](arr1: Array[T1], arr2: Array[T1]): Double
  }

  sealed trait WeightedDistanceMetric[T2] {
    def distance[T1](arr1: Array[T1], arr2: Array[T1], weight: T2): Int
    def distance[T1](arr1: Array[T1], arr2: Array[T1]): Int
  }

  sealed trait ScoreFromDistanceMetric {
    def distance[T](arr1: Array[T], arr2: Array[T]): Int
    def score[T](s1: Array[T], s2: Array[T]): Double = {
      wrapScore[T](
        s1,
        s2,
        () => {
          val maxLen = math.max(s1.length, s2.length)
          val minLen = maxLen - distance(s1, s2)
          (if (minLen < 0 || minLen > maxLen) 0d else minLen * 1d) / maxLen
        })
    }
  }

  object Cosine extends ScoreMetric with CosSimilarityImpl {
    override def score[T](arr1: Array[T], arr2: Array[T]): Double =
      wrapScore(arr1, arr2, () => cosSimilarity(arr1, arr2))
  }

  object Damerau extends ScoreFromDistanceMetric with LevenshteinDistanceImpl {
    override def distance[T](arr1: Array[T], arr2: Array[T]): Int =
      wrapDist(arr1, arr2, () => damerauLevenshtein(arr1, arr2))
  }

  object DiceCoefficient extends ScoreMetric with DiceCoefficientImpl {
    override def score[T](arr1: Array[T], arr2: Array[T]): Double =
      wrapScore(arr1, arr2, () => diceCoefficient(arr1, arr2))
  }

  object Hamming extends ScoreFromDistanceMetric with HammingImpl {
    override def distance[T](arr1: Array[T], arr2: Array[T]): Int =
      wrapDist(arr1, arr2, () => hamming(arr1, arr2))
  }

  object Jaccard extends WeightedScoreMetric[Int] with JaccardImpl {
    override def score[T](arr1: Array[T], arr2: Array[T]): Double =
      wrapScore(arr1, arr2, () => jaccard(arr1, arr2))
    override def score[T](arr1: Array[T], arr2: Array[T], weight: Int): Double =
      wrapScore(arr1, arr2, () => jaccard(arr1, arr2, weight))
  }

  object Jaro extends ScoreMetric with JaroImpl {
    override def score[T](arr1: Array[T], arr2: Array[T]): Double =
      wrapScore(arr1, arr2, () => jaro(arr1, arr2))
  }

  object JaroWinkler extends WeightedScoreMetric[Double] with JaroImpl {
    override def score[T1](arr1: Array[T1], arr2: Array[T1]): Double =
      wrapScore(arr1, arr2, () => jaroWinkler(arr1, arr2))

    override def score[T1](arr1: Array[T1], arr2: Array[T1], weight: Double): Double =
      wrapScore(arr1, arr2, () => jaroWinkler(arr1, arr2, weight))
  }

  object Levenshtein extends ScoreFromDistanceMetric with LevenshteinDistanceImpl {
    override def distance[T](arr1: Array[T], arr2: Array[T]): Int =
      wrapDist(arr1, arr2, () => levenshtein(arr1, arr2))
  }

  object LongestCommonSeq extends DistanceMetric with LongestCommonSeqImpl {
    override def distance[T](arr1: Array[T], arr2: Array[T]): Int =
      wrapDist(arr1, arr2, () => longestCommonSeq(arr1, arr2))
  }

  object NeedlemanWunsch extends WeightedScoreMetric[ConstantGap] with NeedlemanWunschImpl {
    override def score[T1](arr1: Array[T1], arr2: Array[T1]): Double =
      wrapScore(arr1, arr2, () => needleman(arr1, arr2))
    override def score[T1](arr1: Array[T1], arr2: Array[T1], weight: ConstantGap): Double =
      wrapScore(arr1, arr2, () => needleman(arr1, arr2, weight))
  }

  object NGram extends WeightedScoreMetric[Int] with WeightedDistanceMetric[Int] with NGramImpl {
    override def distance[T1](arr1: Array[T1], arr2: Array[T1]): Int =
      wrapDist(arr1, arr2, () => nGramDist(arr1, arr2))
    override def distance[T1](arr1: Array[T1], arr2: Array[T1], weight: Int): Int =
      wrapDist(arr1, arr2, () => nGramDist(arr1, arr2, weight))
    override def score[T1](arr1: Array[T1], arr2: Array[T1]): Double =
      wrapScore(arr1, arr2, () => nGram(arr1, arr2))
    override def score[T1](arr1: Array[T1], arr2: Array[T1], weight: Int): Double =
      wrapScore(arr1, arr2, () => nGram(arr1, arr2, weight))

    def tokens[T](s: Array[T], n: Int): List[List[T]] = tokenizeNGram(s, n)
  }

  object Overlap extends WeightedScoreMetric[Int] with OverlapImpl {
    override def score[T1](arr1: Array[T1], arr2: Array[T1]): Double =
      wrapScore(arr1, arr2, () => overlap(arr1, arr2))
    override def score[T1](arr1: Array[T1], arr2: Array[T1], weight: Int): Double =
      wrapScore(arr1, arr2, () => overlap(arr1, arr2, weight))
  }

  object SmithWaterman extends WeightedScoreMetric[(Gap, Int)] with SmithWatermanImpl {
    override def score[T1](arr1: Array[T1], arr2: Array[T1]): Double =
      wrapScore(arr1, arr2, () => smithWaterman(arr1, arr2))
    override def score[T1](arr1: Array[T1], arr2: Array[T1], n: (Gap, Int)): Double =
      wrapScore(arr1, arr2, () => smithWaterman(arr1, arr2, n._1, n._2))
  }

  object SmithWatermanGotoh extends WeightedScoreMetric[ConstantGap] with SmithWatermanImpl {
    override def score[T1](arr1: Array[T1], arr2: Array[T1]): Double =
      wrapScore(arr1, arr2, () => smithWatermanGotoh(arr1, arr2))
    override def score[T1](arr1: Array[T1], arr2: Array[T1], weight: ConstantGap): Double =
      wrapScore(arr1, arr2, () => smithWatermanGotoh(arr1, arr2, weight))
  }

  object Tversky extends WeightedScoreMetric[Double] with JaccardImpl {
    override def score[T1](arr1: Array[T1], arr2: Array[T1]): Double =
      wrapScore(arr1, arr2, () => tversky(arr1, arr2))
    override def score[T1](arr1: Array[T1], arr2: Array[T1], weight: Double): Double =
      wrapScore(arr1, arr2, () => tversky(arr1, arr2, weight))
  }
}
