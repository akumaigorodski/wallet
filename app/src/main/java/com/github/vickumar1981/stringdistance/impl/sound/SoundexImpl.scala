package com.github.vickumar1981.stringdistance.impl.sound

import com.github.vickumar1981.stringdistance.interfaces.sound.CommonSoundAlgo._
import com.github.vickumar1981.stringdistance.interfaces.sound.SoundexAlgo

private[stringdistance] trait SoundexImpl {
  private def compare(a: String, b: String): Option[Boolean] =
    if (a.length == 0 || !isAlpha(a.head) || b.length == 0 || !isAlpha(b.head)) None
    else if (a.head.toLower != b.head.toLower) Some(false)
    else
      SoundexAlgo.compute(a).filter(_.length > 0).flatMap { se1 =>
        SoundexAlgo.compute(b).filter(_.length > 0).map(se1.sameElements(_))
      }

  protected def soundex(a: String, b: String): Boolean =
    compare(a, b).getOrElse(false)
}
