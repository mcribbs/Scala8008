package net.mcribbs.s8008

import scala.math.Ordering.BooleanOrdering

case class Flags(
  carry: Boolean = false, 
  zero: Boolean = false, 
  sign: Boolean = false, 
  parity: Boolean = false, 
)
{
  override def toString:String = {
      s"Flags(Carry:" + carry + " Zero:" + zero + " Sign:" + sign + " Parity:" + parity
  }
}

object Flags:
  def calcZSP(value: Byte): Flags = {
    val z = value == 0
    val s = (value & 0x80) == 0x80
    val p = value.toInt.toBinaryString.count(_ == '1') % 2 == 0
    Flags(false, z, s, p)
  }