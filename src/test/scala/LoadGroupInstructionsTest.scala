package net.mcribbs.s8008

import org.scalatest.*
import org.scalatest.wordspec.*
import spire.implicits.ub
import spire.math.UByte

class LoadGroupInstructionsTest extends AnyWordSpec {

  "LMI" should {
    "load the memory address in HL with data B...B" in {
      val ram = Memory(new Array[UByte](Memory.MAX_MEMORY)).writeByte(0x0000, UByte(0x3E)).writeByte(0x0001, UByte(0x42))
      val cpu = new CPU(ram)
      cpu.state = cpu.state
        .withRegister(Register.H, UByte(0x00)).withRegister(Register.L, UByte(0x01))
      val state = cpu.step
      assert(state.ram.readByte(0x0001) == UByte(0x42))
    }
  }

  "LrI" should {
    "load the index register with data B...B" in {
      val ram = Memory(new Array[UByte](Memory.MAX_MEMORY)).writeByte(0x0000, UByte(0x06)).writeByte(0x0001, UByte(0x42))
      val cpu = new CPU(ram)
      val state = cpu.step
      assert(state.getRegister(Register.A) == UByte(0x42))
    }
  }

  "LMr" should {
    "load the memory address in HL with the contents of the index register" in {
      val ram = Memory(new Array[UByte](Memory.MAX_MEMORY)).writeByte(0x0000, UByte(0xF8))
      val cpu = new CPU(ram)
      cpu.state = cpu.state.withRegister(Register.A, UByte(0x42))
        .withRegister(Register.H, UByte(0x00)).withRegister(Register.L, UByte(0x01))
      val state = cpu.step
      assert(state.ram.readByte(0x0001) == UByte(0x42))
    }
  }

  "LrM" should {
    "load the index register with the contents of the memory address in HL" in {
      val ram = Memory(new Array[UByte](Memory.MAX_MEMORY)).writeByte(0x0000, UByte(0xC7)).writeByte(0x0001, UByte(0x42))
      val cpu = new CPU(ram)
      cpu.state = cpu.state
        .withRegister(Register.H, UByte(0x00)).withRegister(Register.L, UByte(0x01))
      val state = cpu.step
      assert(state.getRegister(Register.A) == UByte(0x42))
    }
  }

  "Lr1r2" should {
    "load the index register r1 with the contents of index register r2" in {
      val ram = Memory(new Array[UByte](Memory.MAX_MEMORY)).writeByte(0x0000, UByte(0xC8))
      val cpu = new CPU(ram)
      cpu.state = cpu.state.withRegister(Register.A, UByte(0x42))
      val state = cpu.step
      assert(state.getRegister(Register.B) == UByte(0x42))
    }
  }
}