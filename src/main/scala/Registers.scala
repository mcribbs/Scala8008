package net.mcribbs.s8008

class Registers(A: Byte = 0, B: Byte = 0, C: Byte = 0, D: Byte = 0, E: Byte = 0, H: Byte = 0, L: Byte = 0):
  
  // BLECH!!! TODO do this in a way that doesn't make me vomit
  def withRegister(registerId: Byte, value: Byte): Registers = {
    registerId match {
      case Registers.ID.A => new Registers(value,this.B,this.C,this.D,this.E,this.H,this.L)
      case Registers.ID.B => new Registers(this.A,value,this.C,this.D,this.E,this.H,this.L)
      case Registers.ID.C => new Registers(this.A,this.B,value,this.D,this.E,this.H,this.L)
      case Registers.ID.D => new Registers(this.A,this.B,this.C,value,this.E,this.H,this.L)
      case Registers.ID.E => new Registers(this.A,this.B,this.C,this.D,value,this.H,this.L)
      case Registers.ID.H => new Registers(this.A,this.B,this.C,this.D,this.E,value,this.L)
      case Registers.ID.L => new Registers(this.A,this.B,this.C,this.D,this.E,this.H,value)
    }
  }

  def HL: Short = {
    ((H << 8) + L).asInstanceOf[Short]
  }
  
  override def toString:String = {
    f"Registers(A:$A%#04x " +
    f"B:$B%#04x " +
    f"C:$C%#04x " +
    f"D:$D%#04x " +
    f"E:$E%#04x " +
    f"HL:$HL%#06x)"
  }

object Registers:
  object ID:
    val A: Byte = 0
    val B: Byte = 1
    val C: Byte = 2
    val D: Byte = 3
    val E: Byte = 4
    val H: Byte = 5
    val L: Byte = 6