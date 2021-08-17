package com.github.vickumar1981.stringdistance.impl

private[stringdistance] trait HammingImpl {
  protected def hamming[T](s1: Array[T], s2: Array[T]): Int = {
    if (s1.length != s2.length) -1
    else {
      s1.zip(s2).map { case (ch1, ch2) => if (ch1 == ch2) 0 else 1 }.sum
    }
  }
}
