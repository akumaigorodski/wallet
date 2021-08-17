package com.github.vickumar1981.stringdistance.interfaces.sound

private[stringdistance] object CommonSoundAlgo {
  final val alphabet: Set[Char] = ('a' to 'z').toSet ++ ('A' to 'Z').toSet
  final val lowercaseVowel: Set[Char] = Set('a', 'e', 'i', 'o', 'u')

  def isAlpha(ch: Char): Boolean = alphabet.contains(ch)
}
