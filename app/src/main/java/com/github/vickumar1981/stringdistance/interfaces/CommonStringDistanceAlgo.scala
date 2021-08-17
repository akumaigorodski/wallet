package com.github.vickumar1981.stringdistance.interfaces

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

private[stringdistance] trait CommonStringDistanceAlgo {
  final private lazy val MIN_PREFIX_LENGTH = 4

  protected def minStartPrefix[T](
      s1: Array[T],
      s2: Array[T],
      minPrefixLen: Int = MIN_PREFIX_LENGTH): Int = {
    var isSame = true
    var minPrefix = 0
    s1.zipWithIndex.foreach {
      case (ch, chIndex) => {
        if (isSame && chIndex < s2.length && ch == s2(chIndex))
          minPrefix += 1
        else
          isSame = false
      }
    }
    Math.min(minPrefix, minPrefixLen)
  }

  protected def getCommonChars[T](s1: List[T], s2: List[T], halfLen: Int): List[T] = {
    val commonChars: mutable.ListBuffer[T] = ListBuffer.empty[T]
    val strCopy: mutable.ListBuffer[T] = ListBuffer(s2: _*)
    var n = s1.length
    val m = s2.length
    s1.zipWithIndex.foreach {
      case (ch, chIndex) => {
        var foundIt = false
        var j = math.max(0, chIndex - halfLen)
        while (!foundIt && j <= Math.min(chIndex + halfLen, m - 1)) {
          if (strCopy(j) == ch) {
            foundIt = true
            commonChars += ch
          }
          j += 1
        }
      }
    }
    commonChars.toList
  }
}
