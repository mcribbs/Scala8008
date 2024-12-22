package net.mcribbs.s8008

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
  def withRegister(registerId: Byte, value: Byte): CPUState = copy(registers = registers.withRegister(registerId, value))
  def getRegister(registerId: Byte): Byte = registers.getRegister(registerId)
  def writeByte(address: Short, value: Byte): CPUState = copy(ram = ram.writeByte(address, value))

  def logState(): Unit = {
    println(stack.toString)
    println(registers.toString)
    println(flags.toString)
    println("")
  }
}