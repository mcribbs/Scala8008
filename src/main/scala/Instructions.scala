package net.mcribbs.s8008

class Instructions(state: CPUState):

  def HLT: CPUState = 
    state.copy(halt = true)

  def JMP: CPUState =
    val low: Byte = state.ram.readByte(state.PC)
    val state1 = state.incrementPC
    val high: Byte = state1.ram.readByte(state1.PC)
    state1.incrementPC
    state.copy(stack = state.stack.withPC(((high << 8) + low).asInstanceOf[Short]))

  def JFC: CPUState = if(!state.flags.carry) then JMP else state.incrementPC.incrementPC
  def JFZ: CPUState = if(!state.flags.zero) then JMP else state.incrementPC.incrementPC
  def JFS: CPUState = if(!state.flags.sign) then JMP else state.incrementPC.incrementPC
  def JFP: CPUState = if(!state.flags.parity) then JMP else state.incrementPC.incrementPC
  def JC: CPUState = if(state.flags.carry) then JMP else state.incrementPC.incrementPC
  def JZ: CPUState = if(state.flags.zero) then JMP else state.incrementPC.incrementPC
  def JS: CPUState = if(state.flags.sign) then JMP else state.incrementPC.incrementPC
  def JP: CPUState = if(state.flags.parity) then JMP else state.incrementPC.incrementPC

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
