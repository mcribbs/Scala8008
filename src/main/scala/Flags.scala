package net.mcribbs.s8008

import scala.math.Ordering.BooleanOrdering

case class Flags(
  carry: Boolean = false, 
  zero: Boolean = false, 
  sign: Boolean = false, 
  parity: Boolean = false, 
  halt: Boolean = false
)
{
  override def toString:String = {
      s"Flags(Carry:" + carry + " Zero:" + zero + " Sign:" + sign + " Parity:" + parity + " Halt:" + halt
  }
}
