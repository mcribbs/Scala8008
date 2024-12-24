package net.mcribbs.s8008

class Instructions(state: CPUState):

  def HLT: CPUState = 
    state.copy(halt = true)

  def JMP(test: Boolean): CPUState =
    if (test)
      val address = state.ram.readAddress(state.PC)
      state.copy(stack = state.stack.withPC(address))
    else
      state.incrementPC.incrementPC

  def CAL(test: Boolean): CPUState =
    if (test)
      val address = state.ram.readAddress(state.PC)
      val newState = state.incrementPC.incrementPC
      newState.copy(stack = newState.stack.push(address))
    else
      state.incrementPC.incrementPC

  def RET(test: Boolean): CPUState =
    if (test)
      state.decrementSP
    else
      state

  def RST(aaa: Byte): CPUState =
    state.copy(stack = state.stack.push((aaa << 3).toShort))

  def LMI: CPUState =
    val data: Byte = state.ram.readByte(state.PC)
    state.incrementPC.writeByte(state.registers.HL, data)

  def INr(dest: Byte): CPUState =
    val value = (state.getRegister(dest) + 1).toByte
    state.withRegister(dest, value).copy(flags = Flags.calcZSP(value))

  def DCr(dest: Byte): CPUState =
    state.withRegister(dest, (state.getRegister(dest) - 1).toByte)

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
