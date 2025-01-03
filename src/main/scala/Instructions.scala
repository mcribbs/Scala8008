package net.mcribbs.s8008

import spire.math.UByte 

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

  def RST(aaa: UByte): CPUState =
    state.copy(stack = state.stack.push((aaa << 3).toShort))

  def LMI: CPUState =
    val data: UByte = state.ram.readByte(state.PC)
    state.incrementPC.writeByte(state.registers.HL, data)

  def INr(dest: Register): CPUState =
    val value = state.getRegister(dest) + 0x01
    state.withRegister(dest, value).copy(flags = Flags.calcZSP(value))

  def DCr(dest: Register): CPUState =
    state.withRegister(dest, state.getRegister(dest) - 0x01)

  def LrI(dest: Register): CPUState =
    val data: UByte = state.ram.readByte(state.PC)
    state.incrementPC.withRegister(dest, data)

  def LMr(source: Register): CPUState =
    val data = state.getRegister(source)
    state.writeByte(state.registers.HL, data)

  def LrM(dest: Register): CPUState =
    val data: UByte = state.ram.readByte(state.registers.HL)
    state.incrementPC.withRegister(dest, data)

  def Lrr(dest: Register, source: Register): CPUState =
    state.withRegister(dest, state.getRegister(source))

  def ADM: CPUState = doALU(ADDRESSING_MODE.M, Register.A, add)
  def ADI: CPUState = doALU(ADDRESSING_MODE.I, Register.A, add)
  def ADr(s: Register): CPUState = doALU(ADDRESSING_MODE.R, s, add)

  def ACM: CPUState = doALU(ADDRESSING_MODE.M, Register.A, addWithCarry)
  def ACI: CPUState = doALU(ADDRESSING_MODE.I, Register.A, addWithCarry)
  def ACr(s: Register): CPUState = doALU(ADDRESSING_MODE.R, s, addWithCarry)

  def SUM: CPUState = doALU(ADDRESSING_MODE.M, Register.A, subtract)
  def SUI: CPUState = doALU(ADDRESSING_MODE.I, Register.A, subtract)
  def SUr(s: Register): CPUState = doALU(ADDRESSING_MODE.R, s, subtract)

  def SBM: CPUState = doALU(ADDRESSING_MODE.M, Register.A, subtractWithBorrow)
  def SBI: CPUState = doALU(ADDRESSING_MODE.I, Register.A, subtractWithBorrow)
  def SBr(s: Register): CPUState = doALU(ADDRESSING_MODE.R, s, subtractWithBorrow)

  def NDM: CPUState = doALU(ADDRESSING_MODE.M, Register.A, (a, b) => (a & b, false))
  def NDI: CPUState = doALU(ADDRESSING_MODE.I, Register.A, (a, b) => (a & b, false))
  def NDr(s: Register): CPUState = doALU(ADDRESSING_MODE.R, s, (a, b) => (a & b, false))

  def XRM: CPUState = doALU(ADDRESSING_MODE.M, Register.A, (a, b) => (a ^ b, false))
  def XRI: CPUState = doALU(ADDRESSING_MODE.I, Register.A, (a, b) => (a ^ b, false))
  def XRr(s: Register): CPUState = doALU(ADDRESSING_MODE.R, s, (a, b) => (a ^ b, false))

  def ORM: CPUState = doALU(ADDRESSING_MODE.M, Register.A, (a, b) => (a | b, false))
  def ORI: CPUState = doALU(ADDRESSING_MODE.I, Register.A, (a, b) => (a | b, false))
  def ORr(s: Register): CPUState = doALU(ADDRESSING_MODE.R, s, (a, b) => (a | b, false))

  def CPM: CPUState = doALU(ADDRESSING_MODE.M, Register.A, compare)
  def CPI: CPUState = doALU(ADDRESSING_MODE.I, Register.A, compare)
  def CPr(s: Register): CPUState = doALU(ADDRESSING_MODE.R, s, compare)

  def RLC: CPUState =
    val shifted = state.getRegister(Register.A).toInt << 1
    val c = shifted > 255
    state.withRegister(Register.A, shifted | c.compare(false)).copy(flags = state.flags.withFlag(Flag.C, c))

  def RRC: CPUState =
    val c = state.getRegister(Register.A) & 0x01
    val shifted = state.getRegister(Register.A).toInt >>> 1
    state.withRegister(Register.A, shifted | c << 7).copy(flags = state.flags.withFlag(Flag.C, c.toInt != 0))

  def RAL: CPUState =
    val shifted = state.getRegister(Register.A).toInt << 1
    val c = shifted > 255
    state.withRegister(Register.A, shifted | state.flags.carry.compare(false)).copy(flags = state.flags.withFlag(Flag.C, c))

  def RAR: CPUState =
    val c = state.getRegister(Register.A) & 0x01
    val shifted = state.getRegister(Register.A).toInt >>> 1
    state.withRegister(Register.A, shifted | state.flags.carry.compare(false) << 7).copy(flags = state.flags.withFlag(Flag.C, c.toInt != 0))
    
  private def add(a: UByte, b: UByte): (UByte, Boolean) =
    val c = a + b
    val carry = (c < a) || (c < b)
    (c, carry)

  private def addWithCarry(a: UByte, b: UByte): (UByte, Boolean) =
    val c = a + b + UByte(state.flags.carry.compare(false))
    val carry = (c < a) || (c < b)
    (c, carry)

  private def subtract(a: UByte, b: UByte): (UByte, Boolean) =
    val c = a - b
    val carry = (c > a) && (c > b)
    (c, carry)

  private def subtractWithBorrow(a: UByte, b: UByte): (UByte, Boolean) =
    val c = a - b - UByte(state.flags.carry.compare(false))
    val carry = (c > a) && (c > b)
    (c, carry)

  private def compare(a: UByte, b: UByte): (UByte, Boolean) =
    val c = a - b
    val carry = (c > a) && (c > b)
    (a, carry)

  private def doALU(mode: ADDRESSING_MODE, source: Register, f: (UByte, UByte) => (UByte, Boolean)): CPUState =
    val (data: UByte, s: CPUState) = mode match {
      case ADDRESSING_MODE.M => (state.ram.readByte(state.registers.HL), state.incrementPC)
      case ADDRESSING_MODE.I => (state.ram.readByte(state.PC), state.incrementPC)
      case ADDRESSING_MODE.R => (state.getRegister(source), state)
    }
    val (answer, carry) = f(s.getRegister(Register.A), data)
    s.withRegister(Register.A, answer)
      .copy(flags = Flags.calcZSP(answer).copy(carry = carry))

enum ADDRESSING_MODE:
  case M, I, R