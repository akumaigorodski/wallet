package com.github.vickumar1981.stringdistance

import com.github.vickumar1981.stringdistance.impl.sound.{MetaphoneImpl, SoundexImpl}

/**
 * Main class to organize functionality of different phonetic/sound string algorithms
 *
 * {{{
 * import com.github.vickumar1981.stringdistance.StringSound._
 * import com.github.vickumar1981.stringdistance.implicits._
 *
 * // Phonetic similarity between strings
 * val metaphone: Boolean = Metaphone.score("merci", "mercy")
 * val soundex: Boolean = Soundex.score("merci", "mercy")
 * }}}
 */
object StringSound {
  object Metaphone extends StringSoundMetric[MetaphoneAlgorithm]
  object Soundex extends StringSoundMetric[SoundexAlgorithm]
}

/**
 * Java Wrapper for metaphone similarity.
 */
class MetaphoneImplWrapper extends MetaphoneImpl

/**
 * Java Wrapper for soundex similarity.
 */
class SoundexImplWrapper extends SoundexImpl
