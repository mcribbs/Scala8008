package net.mcribbs.s8008

import spire.math.UByte

class Registers(A: UByte = UByte(0x00), B: UByte = UByte(0x00), C: UByte = UByte(0x00), D: UByte = UByte(0x00),
                E: UByte = UByte(0x00), H: UByte = UByte(0x00), L: UByte = UByte(0x00)):
  
  // BLECH!!! TODO do this in a way that doesn't make me vomit
  def withRegister(registerId: Register, value: UByte): Registers = {
    registerId match {
      case Register.A => new Registers(value,this.B,this.C,this.D,this.E,this.H,this.L)
      case Register.B => new Registers(this.A,value,this.C,this.D,this.E,this.H,this.L)
      case Register.C => new Registers(this.A,this.B,value,this.D,this.E,this.H,this.L)
      case Register.D => new Registers(this.A,this.B,this.C,value,this.E,this.H,this.L)
      case Register.E => new Registers(this.A,this.B,this.C,this.D,value,this.H,this.L)
      case Register.H => new Registers(this.A,this.B,this.C,this.D,this.E,value,this.L)
      case Register.L => new Registers(this.A,this.B,this.C,this.D,this.E,this.H,value)
    }
  }
  
  def getRegister(registerId: Register): UByte = {
    registerId match {
      case Register.A => A
      case Register.B => B
      case Register.C => C
      case Register.D => D
      case Register.E => E
      case Register.H => H
      case Register.L => L
    }
  }

  def HL: Short = {
    ((H.toShort << 8) + L.toShort).toShort
  }
  
  override def toString:String = {
    f"Registers(A:${A.toInt}%#04x " +
    f"B:${B.toInt}%#04x " +
    f"C:${C.toInt}%#04x " +
    f"D:${D.toInt}%#04x " +
    f"E:${E.toInt}%#04x " +
    f"H:${H.toInt}%#04x " +
    f"L:${L.toInt}%#04x " +
    f"HL:${HL}%#06x)"
  }

object Registers: 
  def decodeRegister(r: UByte): Register =
    r.toInt match {
      case 0 => Register.A
      case 1 => Register.B
      case 2 => Register.C
      case 3 => Register.D
      case 4 => Register.E
      case 5 => Register.H
      case 6 => Register.L
      case 7 => Register.HL
    }
  
enum Register:
    case A, B, C, D, E, H, L, HL