package net.mcribbs.s8008

import scala.math.Ordering.BooleanOrdering
import spire.math.UByte

case class Flags(
  carry: Boolean = false, 
  zero: Boolean = false, 
  sign: Boolean = false, 
  parity: Boolean = false, 
)
{
  def withFlag(flagId: Flag, value: Boolean): Flags = {
    flagId match {
      case Flag.C => new Flags(value,this.zero,this.sign,this.parity)
      case Flag.Z => new Flags(this.carry,value,this.sign,this.parity)
      case Flag.S => new Flags(this.carry,this.zero,value,this.parity)
      case Flag.P => new Flags(this.carry,this.zero,this.sign,value)
    }
  }
  override def toString:String = {
      s"Flags(Carry:" + carry + " Zero:" + zero + " Sign:" + sign + " Parity:" + parity
  }
}

object Flags:
  def calcZSP(value: UByte): Flags = {
    val z = value == UByte(0x00)
    val s = (value & UByte(0x80)) == UByte(0x80)
    val p = value.toInt.toBinaryString.count(_ == '1') % 2 == 0
    Flags(false, z, s, p)
  }

enum Flag:
  case C, Z, S, P