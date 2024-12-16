package net.mcribbs.s8008

import net.mcribbs.s8008.Registers

case class CPUState
(
  pc: Short = 0,
  //sp: Int = 0,
  //stack: Array[Short] = Array(0,0,0,0,0,0,0),
  registers: Registers = Registers()
)
{
  def updatePC(value: Short): CPUState = copy(pc = value)
  def withRegister(registerId: Byte, value: Byte): CPUState = copy(registers = registers.withRegister(registerId, value))

  def logState(): Unit = {
    print(f"PC:$pc%#06x ")
    print(f"Registers(A:${registers.A}%#04x ")
    print(f"B:${registers.B}%#04x ")
    print(f"C:${registers.C}%#04x ")
    print(f"D:${registers.D}%#04x ")
    print(f"E:${registers.E}%#04x ")
    print(f"HL:${registers.HL}%#06x)")
    println("")
  }
}

  