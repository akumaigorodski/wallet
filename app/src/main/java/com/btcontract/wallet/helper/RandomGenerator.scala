package com.btcontract.wallet.helper


class RandomGenerator
extends java.security.SecureRandom
{
  def getBytes(size: Int) = {
    val array = new Array[Byte](size)
    super.nextBytes(array)
    array
  }

  override def nextInt = {
    val tmpBuffer = getBytes(4)
    val x1 = (tmpBuffer(0) & 0xff) << 24
    val x2 = (tmpBuffer(1) & 0xff) << 16
    val x3 = (tmpBuffer(2) & 0xff) << 8
    val x4 = tmpBuffer(3) & 0xff
    x1 | x2 | x3 | x4
  }
}
