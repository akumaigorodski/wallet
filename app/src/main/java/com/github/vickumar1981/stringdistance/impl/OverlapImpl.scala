package com.github.vickumar1981.stringdistance.impl

import com.github.vickumar1981.stringdistance.interfaces.NGramTokenizer

private[stringdistance] trait OverlapImpl extends NGramTokenizer {
  protected def overlap[T](s1: Array[T], s2: Array[T], n: Int = 1): Double = {
    require(n > 0, "Overlap score, ngram size must be a positive number.")
    foldNGram(s1, s2, n)(0d)(_ => 1d) { (s1Tok, s2Tok, dist) =>
      dist.toDouble / math.min(s1Tok.length, s2Tok.length)
    }
  }
}
