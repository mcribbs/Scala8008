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

  def ADM: CPUState = doALU(ADDRESSING_MODE.M, 0, add)
  def ADI: CPUState = doALU(ADDRESSING_MODE.I, 0, add)
  def ADr(s: Byte): CPUState = doALU(ADDRESSING_MODE.R, s, add)

  private def add(a: Byte, b: Byte): (Byte, Boolean) =
    val c = a + b
    val carry = (c < a) || (c < b)
    (c.toByte, carry)

  private def doALU(mode: ADDRESSING_MODE, source: Byte, f: (Byte, Byte) => (Byte, Boolean)): CPUState =
    val (data:Byte, s: CPUState) = mode match {
      case ADDRESSING_MODE.M => (state.ram.readByte(state.registers.HL), state.incrementPC)
      case ADDRESSING_MODE.I => (state.ram.readByte(state.PC), state.incrementPC)
      case ADDRESSING_MODE.R => (state.getRegister(source), state)
    }
    val (answer, carry) = f(s.getRegister(Registers.ID.A), data)
    s.withRegister(Registers.ID.A, answer)
      .copy(flags = Flags.calcZSP(answer).copy(carry = carry))

enum ADDRESSING_MODE:
  case M, I, R