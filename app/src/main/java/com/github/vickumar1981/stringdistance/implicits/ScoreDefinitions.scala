package com.github.vickumar1981.stringdistance.implicits

import com.github.vickumar1981.stringdistance._
import com.github.vickumar1981.stringdistance.impl._

trait ScoreDefinitions {

  /**
   * Implicit definition of cosine similarity score for [[CosineAlgorithm]].
   */
  implicit object CosSimilarityScore
      extends CosSimilarityImpl
      with ScoringAlgorithm[CosineAlgorithm] {

    /**
     * The score method takes two strings and returns the cosine similarity between them.
     *
     * @param s1 The 1st String.
     * @param s2 The 2nd String.
     * @return Returns the cosine similarity between Strings s1 and s2.
     */
    override def score(s1: String, s2: String): Double =
      cosSimilarity(s1.toCharArray, s2.toCharArray)
  }

  /**
   * Implicit definition of dice coefficient score for [[DiceCoefficientAlgorithm]].
   */
  implicit object DiceCoefficientScore
      extends DiceCoefficientImpl
      with ScoringAlgorithm[DiceCoefficientAlgorithm] {

    /**
     * The score method takes two strings and returns the dice coefficient score between them.
     *
     * @param s1 The 1st String.
     * @param s2 The 2nd String.
     * @return Returns the dice coefficient score between Strings s1 and s2.
     */
    override def score(s1: String, s2: String): Double =
      diceCoefficient(s1.toCharArray, s2.toCharArray)
  }

  /**
   * Implicit definition of jaccard score for [[JaccardAlgorithm]].
   */
  implicit object JaccardScore
      extends JaccardImpl
      with WeightedScoringAlgorithm[JaccardAlgorithm, Int] {

    /**
     * The score method takes two strings and returns jaccard score between them.
     *
     * @param s1 The 1st String.
     * @param s2 The 2nd String.
     * @return Returns the jaccard score between Strings s1 and s2.
     */
    override def score(s1: String, s2: String, n: Int): Double =
      jaccard(s1.toCharArray, s2.toCharArray, n)
    override def score(s1: String, s2: String): Double = jaccard(s1.toCharArray, s2.toCharArray)
  }

  /**
   * Implicit definition of jaro score for [[JaroAlgorithm]].
   */
  implicit object JaroScore extends JaroImpl with ScoringAlgorithm[JaroAlgorithm] {

    /**
     * The score method takes two strings and returns the jaro score between them.
     *
     * @param s1 The 1st String.
     * @param s2 The 2nd String.
     * @return Returns the jaro score between Strings s1 and s2.
     */
    override def score(s1: String, s2: String): Double = jaro(s1.toCharArray, s2.toCharArray)
  }

  /**
   * Implicit definition of jaro winkler score for [[JaroWinklerAlgorithm]].
   */
  implicit object JaroWinklerScore
      extends JaroImpl
      with WeightedScoringAlgorithm[JaroWinklerAlgorithm, Double] {

    /**
     * The score method takes two strings and returns the jaro winkler score between them.
     *
     * @param s1 The 1st String.
     * @param s2 The 2nd String.
     * @return Returns the jaro winkler score between Strings s1 and s2.
     */
    override def score(s1: String, s2: String, weight: Double = 0.1): Double =
      jaroWinkler(s1.toCharArray, s2.toCharArray, weight)
    override def score(s1: String, s2: String): Double = jaroWinkler(s1.toCharArray, s2.toCharArray)
  }

  /**
   * Implicit definition of needleman wunsch score for [[NeedlemanWunschAlgorithm]].
   */
  implicit object NeedlemanWunschScore
      extends NeedlemanWunschImpl
      with WeightedScoringAlgorithm[NeedlemanWunschAlgorithm, ConstantGap] {

    /**
     * The score method takes two strings and returns needleman wunsch similarity between them.
     *
     * @param s1 The 1st String.
     * @param s2 The 2nd String.
     * @return Returns the needleman wunsch similarity between Strings s1 and s2.
     */
    override def score(s1: String, s2: String, gap: ConstantGap = ConstantGap()): Double =
      needleman(s1.toCharArray, s2.toCharArray, gap)
    override def score(s1: String, s2: String): Double = needleman(s1.toCharArray, s2.toCharArray)
  }

  /**
   * Implicit definition of n-gram score for [[NGramAlgorithm]].
   */
  implicit object NGramScore extends NGramImpl with WeightedScoringAlgorithm[NGramAlgorithm, Int] {

    /**
     * The score method takes two strings and returns n-gram similarity between them.
     *
     * @param s1 The 1st String.
     * @param s2 The 2nd String.
     * @return Returns the n-gram similarity between Strings s1 and s2.
     */
    override def score(s1: String, s2: String, n: Int): Double =
      nGram(s1.toCharArray, s2.toCharArray, n)
    override def score(s1: String, s2: String): Double = nGram(s1.toCharArray, s2.toCharArray)
  }

  /**
   * Implicit definition of overlap score for [[OverlapAlgorithm]].
   */
  implicit object OverlapScore
      extends OverlapImpl
      with WeightedScoringAlgorithm[OverlapAlgorithm, Int] {

    /**
     * The score method takes two strings and returns n-gram similarity between them.
     *
     * @param s1 The 1st String.
     * @param s2 The 2nd String.
     * @return Returns the overlap similarity between Strings s1 and s2.
     */
    override def score(s1: String, s2: String, n: Int = 1): Double =
      overlap(s1.toCharArray, s2.toCharArray, n)
    override def score(s1: String, s2: String): Double = overlap(s1.toCharArray, s2.toCharArray)
  }

  /**
   * Implicit definition of smith waterman score for [[SmithWatermanAlgorithm]].
   */
  implicit object SmithWatermanScore
      extends SmithWatermanImpl
      with WeightedScoringAlgorithm[SmithWatermanAlgorithm, (Gap, Int)] {

    /**
     * The score method takes two strings and returns smith waterman similarity between them.
     *
     * @param s1 The 1st String.
     * @param s2 The 2nd String.
     * @return Returns the smith waterman similarity between Strings s1 and s2.
     */
    override def score(s1: String, s2: String, gapAndWindowSize: (Gap, Int)): Double =
      smithWaterman(s1.toCharArray, s2.toCharArray, gapAndWindowSize._1, gapAndWindowSize._2)
    override def score(s1: String, s2: String): Double =
      smithWaterman(s1.toCharArray, s2.toCharArray)
  }

  /**
   * Implicit definition of smith waterman gotoh score for [[SmithWatermanGotohAlgorithm]].
   */
  implicit object SmithWatermanGotohScore
      extends SmithWatermanImpl
      with WeightedScoringAlgorithm[SmithWatermanGotohAlgorithm, ConstantGap] {

    /**
     * The score method takes two strings and returns smith waterman similarity between them.
     *
     * @param s1 The 1st String.
     * @param s2 The 2nd String.
     * @return Returns the smith waterman gotoh similarity between Strings s1 and s2.
     */
    override def score(s1: String, s2: String, gap: ConstantGap): Double =
      smithWatermanGotoh(s1.toCharArray, s2.toCharArray, gap)
    override def score(s1: String, s2: String): Double =
      smithWatermanGotoh(s1.toCharArray, s2.toCharArray)
  }

  /**
   * Implicit definition of tversky score for [[TverskyAlgorithm]].
   */
  implicit object TverskyScore
      extends JaccardImpl
      with WeightedScoringAlgorithm[TverskyAlgorithm, Double] {

    /**
     * The score method takes two strings and returns tversky score between them.
     *
     * @param s1 The 1st String.
     * @param s2 The 2nd String.
     * @return Returns the tversky score between Strings s1 and s2.
     */
    override def score(s1: String, s2: String, n: Double): Double =
      tversky(s1.toCharArray, s2.toCharArray, n)
    override def score(s1: String, s2: String): Double = tversky(s1.toCharArray, s2.toCharArray)
  }
}
