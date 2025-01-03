package net.mcribbs.s8008

import org.scalatest.*
import org.scalatest.wordspec.*
import spire.math.UByte

class RotateGroupInstructionsTest extends AnyWordSpec {

  "RLC" should {
    "rotate the content of the accumulator left" in {
      val ram = Memory(new Array[UByte](Memory.MAX_MEMORY)).writeByte(0x0000, UByte(0x02))
      val cpu = new CPU(ram)
      cpu.state = cpu.state.withRegister(Register.A, 0xD2)
      val state = cpu.step
      assert(state.getRegister(Register.A) == UByte(0xA5))
      assert(state.flags.carry)
    }
  }

  "RRC" should {
    "rotate the content of the accumulator right" in {
      val ram = Memory(new Array[UByte](Memory.MAX_MEMORY)).writeByte(0x0000, UByte(0x0A))
      val cpu = new CPU(ram)
      cpu.state = cpu.state.withRegister(Register.A, 0x45)
      val state = cpu.step
      assert(state.getRegister(Register.A) == UByte(0xA2))
      assert(state.flags.carry)
    }
  }

  "RAL" should {
    "rotate the content of the accumulator left through the carry flag" in {
      val ram = Memory(new Array[UByte](Memory.MAX_MEMORY)).writeByte(0x0000, UByte(0x12))
      val cpu = new CPU(ram)
      cpu.state = cpu.state.withRegister(Register.A, 0x69).withFlag(Flag.C, true)
      val state = cpu.step
      assert(state.getRegister(Register.A) == UByte(0xD3))
      assert(!state.flags.carry)
    }
  }

  "RAL" should {
    "rotate the content of the accumulator right" in {
      val ram = Memory(new Array[UByte](Memory.MAX_MEMORY)).writeByte(0x0000, UByte(0x1A))
      val cpu = new CPU(ram)
      cpu.state = cpu.state.withRegister(Register.A, 0x45).withFlag(Flag.C, true)
      val state = cpu.step
      assert(state.getRegister(Register.A) == UByte(0xA2))
      assert(state.flags.carry)
    }
  }
}