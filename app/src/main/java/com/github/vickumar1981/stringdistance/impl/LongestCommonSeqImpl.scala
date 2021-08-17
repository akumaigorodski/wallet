package com.github.vickumar1981.stringdistance.impl

private[stringdistance] trait LongestCommonSeqImpl {
  protected def longestCommonSeq[T](s1: Array[T], s2: Array[T]): Int = {
    val (s1Len, s2Len) = (s1.length, s2.length)
    val dist = Array.ofDim[Int](s1Len + 1, s2Len + 1)
    for (i <- 1 to s1Len) {
      for (j <- 1 to s2Len) {
        if (s1(i - 1) == s2(j - 1)) {
          dist(i)(j) = dist(i - 1)(j - 1) + 1
        } else {
          dist(i)(j) = math.max(dist(i - 1)(j), dist(i)(j - 1))
        }
      }
    }
    dist(s1Len)(s2Len)
  }
}
