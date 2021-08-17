package com.github.vickumar1981.stringdistance.implicits

import com.github.vickumar1981.stringdistance.{
  MetaphoneAlgorithm,
  SoundScoringAlgorithm,
  SoundexAlgorithm
}
import com.github.vickumar1981.stringdistance.impl.sound.{MetaphoneImpl, SoundexImpl}

trait SoundDefinitions {

  /**
   * Implicit definition of metaphone score for [[MetaphoneAlgorithm]].
   */
  implicit object MetaphoneScore
      extends MetaphoneImpl
      with SoundScoringAlgorithm[MetaphoneAlgorithm] {

    /**
     * The score method takes two strings and returns whether they sound alike.
     *
     * @param s1 The 1st String.
     * @param s2 The 2nd String.
     * @return Returns true or false if String s1 sounds like string s2.
     */
    override def score(s1: String, s2: String): Boolean = metaphone(s1, s2)
  }

  /**
   * Implicit definition of soundex score for [[SoundexAlgorithm]].
   */
  implicit object SoundexScore extends SoundexImpl with SoundScoringAlgorithm[SoundexAlgorithm] {

    /**
     * The score method takes two strings and returns whether they sound alike.
     *
     * @param s1 The 1st String.
     * @param s2 The 2nd String.
     * @return Returns true or false if String s1 sounds like string s2.
     */
    override def score(s1: String, s2: String): Boolean = soundex(s1, s2)
  }
}
