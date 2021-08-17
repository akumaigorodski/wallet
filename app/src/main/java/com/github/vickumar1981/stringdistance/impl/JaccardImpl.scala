package com.github.vickumar1981.stringdistance.impl

import com.github.vickumar1981.stringdistance.interfaces.NGramTokenizer

private[stringdistance] trait JaccardImpl extends NGramTokenizer {
  protected def jaccard[T](s1: Array[T], s2: Array[T], n: Int = 1): Double = {
    require(n > 0, "Jaccard n-gram size must be a positive number.")
    foldNGram(s1, s2, n)(0d)(_ => 1d) { (s1Tok, s2Tok, dist) =>
      dist.toDouble / (s1Tok.length + s2Tok.length - dist)
    }
  }

  protected def tversky[T](s1: Array[T], s2: Array[T], n: Double = 1): Double = {
    require(n >= 0 && n <= 1, "Tversky weight must be a number between 0 and 1.")
    foldNGram(s1, s2, 2)(0d)(_ => 1d) { (s1Tok, s2Tok, dist) =>
      {
        val s1Complement = s1Tok.map { s => !s2Tok.contains(s) }.filter { identity }
        val s2Complement = s2Tok.map { s => !s1Tok.contains(s) }.filter { identity }
        dist.toDouble / (dist.toDouble + (n * s1Complement.length) + (n * s2Complement.length))
      }
    }
  }
}
