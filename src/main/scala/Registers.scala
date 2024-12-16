package net.mcribbs
package s8008

import net.mcribbs.Registers

case class Registers(A: Byte = 0, B: Byte = 0, C: Byte = 0, D: Byte = 0, E: Byte = 0, H: Byte = 0, L: Byte = 0) {

  def withRegister(registerId: Byte, value: Byte): Registers = {
    registerId match {
      case Registers.ID.A => copy(A = value)
      case Registers.ID.B => copy(B = value)
      case Registers.ID.C => copy(C = value)
      case Registers.ID.D => copy(D = value)
      case Registers.ID.E => copy(E = value)
      case Registers.ID.H => copy(H = value)
      case Registers.ID.L => copy(L = value)
    }
  }

  def HL: Short = {
    ((H << 8) + L).asInstanceOf[Short]
  }
}

case object Registers:
  object ID:
    val A: Byte = 0
    val B: Byte = 1
    val C: Byte = 2
    val D: Byte = 3
    val E: Byte = 4
    val H: Byte = 5
    val L: Byte = 6
