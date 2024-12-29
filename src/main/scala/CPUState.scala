package net.mcribbs.s8008

import spire.math.UByte

case class CPUState (
                      stack: Stack = Stack(), 
                      registers: Registers = Registers(), 
                      flags: Flags = Flags(),
                      halt: Boolean = false,
                      ram: Memory 
                    )
{
  def PC: Short = stack.PC
  def incrementPC: CPUState = copy(stack = stack.incPC)
  def pushToStack(address: Short): CPUState = copy(stack = stack.push(address))
  def decrementSP: CPUState = copy(stack = stack.decSP)
  def withRegister(registerId: Register, value: UByte): CPUState = copy(registers = registers.withRegister(registerId, value))
  def getRegister(registerId: Register): UByte = registers.getRegister(registerId)
  def writeByte(address: Short, value: UByte): CPUState = copy(ram = ram.writeByte(address, value))
  def withFlag(flagId: Flag, value: Boolean): CPUState = copy(flags = flags.withFlag(flagId, value))

  def logState(): Unit = {
    println(stack.toString)
    println(registers.toString)
    println(flags.toString)
    println("")
  }
}