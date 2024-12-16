package net.mcribbs.s8008

import net.mcribbs.s8008.Registers

case class CPUState
(
  stack: Stack = Stack(),
  registers: Registers = Registers()
)
{
  def incrementPC: CPUState = copy(stack = stack.incPC)
  def pushToStack(address: Short): CPUState = copy(stack = stack.push(address))
  def decrementSP: CPUState = copy(stack = stack.decSP)
  def withRegister(registerId: Byte, value: Byte): CPUState = copy(registers = registers.withRegister(registerId, value))

  def logState(): Unit = {
    print(stack.toString)
    print(registers.toString)
    println("")
  }
}

@main def main(): Unit = {

  println("Initial State")
  val initialState = CPUState()
  initialState.logState()

  println("Test Registers")
  val afterRegTests = initialState.withRegister(Registers.ID.H, 0x11).withRegister(Registers.ID.L, 0x05)
  afterRegTests.logState()

  println("Test Stack")
  val afterStackTests = afterRegTests.incrementPC.incrementPC.pushToStack(0xFF).incrementPC
  afterStackTests.logState()
  println("PC is: " + afterStackTests.stack.PC)
  val afterStackTests2 = afterStackTests.decrementSP
  afterStackTests2.logState()
  println("PC is: " + afterStackTests2.stack.PC)

  println("Test Memory")
  val ram = Memory(new Array[Byte](Memory.MAX_MEMORY))
  val newRAM = ram.writeByte(0x0002, 0x42)
  val answer = newRAM.readByte(0x0002)
  println("Value written was: " + answer)
  newRAM.logBytes(0x0000, 64)
}
