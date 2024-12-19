package net.mcribbs.s8008

class Instructions(state: CPUState):

  def HLT: CPUState = 
    state.copy(flags = state.flags.copy(halt = true))
    
  def LMI: CPUState =
    val data: Byte = state.ram.readByte(state.PC)
    state.incrementPC.writeByte(state.registers.HL, data)

  def LrI(dest: Byte): CPUState =
    val data: Byte = state.ram.readByte(state.PC)
    state.incrementPC.withRegister(dest, data)
    
  def LMr(source: Byte): CPUState =
    val data = state.getRegister(source)
    state.writeByte(state.registers.HL, data)
    
  def LrM(dest: Byte): CPUState =
    val data: Byte = state.ram.readByte(state.registers.HL)
    state.incrementPC.withRegister(dest, data)

  def Lrr(dest: Byte, source: Byte): CPUState =
    state.withRegister(dest, state.getRegister(source))
