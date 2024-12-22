package net.mcribbs.s8008

import org.scalatest._
import wordspec._

class InstructionsTest extends AnyWordSpec {

  "HLT" should {
    "set the Halt flag to true for the 0x00 option" in {
      val ram = Memory(new Array[Byte](Memory.MAX_MEMORY)).writeByte(0x0000, 0x00)
      val cpu = new CPU(ram)
      val state = cpu.step
      assert(state.halt)
    }

    "set the Halt flag to true for the 0xFF option" in {
      val ram = Memory(new Array[Byte](Memory.MAX_MEMORY)).writeByte(0x0000, 0xFF.toByte)
      val cpu = new CPU(ram)
      val state = cpu.step
      assert(state.halt)
    }
  }

  "LMI" should {
    "load the memory address in HL with data B...B" in {
      val ram = Memory(new Array[Byte](Memory.MAX_MEMORY)).writeByte(0x0000, 0x3E).writeByte(0x0001, 0x42)
      val cpu = new CPU(ram)
      cpu.state = cpu.state
        .withRegister(Registers.ID.H, 0x00).withRegister(Registers.ID.L, 0x01)
      val state = cpu.step
      assert(state.ram.readByte(0x0001) == 0x42)
    }
  }

  "INr" should {
    "increment the contents of register r and set ZSP" in {
      val ram = Memory(new Array[Byte](Memory.MAX_MEMORY)).writeByte(0x0000, 0x08)
      val cpu = new CPU(ram)
      cpu.state = cpu.state.withRegister(Registers.ID.B, 0xFD.toByte)
      cpu.state = cpu.step
      assert(cpu.state.getRegister(Registers.ID.B) == 0xFE.toByte)
      val flags = Flags.calcZSP(cpu.state.getRegister(Registers.ID.B))
      assert(!flags.zero)
      assert(flags.sign)
      assert(!flags.parity)

      cpu.state = cpu.state.decrementSP.withRegister(Registers.ID.B, 0xFF.toByte)
      cpu.state = cpu.step
      assert(cpu.state.getRegister(Registers.ID.B) == 0x00.toByte)
      val flags2 = Flags.calcZSP(cpu.state.getRegister(Registers.ID.B))
      assert(flags2.zero)
      assert(!flags2.sign)
      assert(flags2.parity)
    }
  }

  "DCr" should {
    "decrement the contents of register r" in {
      val ram = Memory(new Array[Byte](Memory.MAX_MEMORY)).writeByte(0x0000, 0x09)
      val cpu = new CPU(ram)
      cpu.state = cpu.state.withRegister(Registers.ID.B, 0xFF.toByte)
      cpu.state = cpu.step
      assert(cpu.state.getRegister(Registers.ID.B) == 0xFE.toByte)
      val flags = Flags.calcZSP(cpu.state.getRegister(Registers.ID.B))
      assert(!flags.zero)
      assert(flags.sign)
      assert(!flags.parity)

      cpu.state = cpu.state.decrementSP.withRegister(Registers.ID.B, 0x01)
      cpu.state = cpu.step
      assert(cpu.state.getRegister(Registers.ID.B) == 0x00)
      val flags2 = Flags.calcZSP(cpu.state.getRegister(Registers.ID.B))
      assert(flags2.zero)
      assert(!flags2.sign)
      assert(flags2.parity)
    }
  }

  "LrI" should {
    "load the index register with data B...B" in {
      val ram = Memory(new Array[Byte](Memory.MAX_MEMORY)).writeByte(0x0000, 0x06).writeByte(0x0001, 0x42)
      val cpu = new CPU(ram)
      val state = cpu.step
      assert(state.getRegister(Registers.ID.A) == 0x42)
    }
  }

  "LMr" should {
    "load the memory address in HL with the contents of the index register" in {
      val ram = Memory(new Array[Byte](Memory.MAX_MEMORY)).writeByte(0x0000, 0xF8.toByte)
      val cpu = new CPU(ram)
      cpu.state = cpu.state.withRegister(Registers.ID.A, 0x42)
        .withRegister(Registers.ID.H, 0x00).withRegister(Registers.ID.L, 0x01)
      val state = cpu.step
      assert(state.ram.readByte(0x0001) == 0x42)
    }
  }

  "LrM" should {
    "load the index register with the contents of the memory address in HL" in {
      val ram = Memory(new Array[Byte](Memory.MAX_MEMORY)).writeByte(0x0000, 0xC7.toByte).writeByte(0x0001, 0x42)
      val cpu = new CPU(ram)
      cpu.state = cpu.state
        .withRegister(Registers.ID.H, 0x00).withRegister(Registers.ID.L, 0x01)
      val state = cpu.step
      assert(state.getRegister(Registers.ID.A) == 0x42)
    }
  }

  "Lr1r2" should {
    "load the index register r1 with the contents of index register r2" in {
      val ram = Memory(new Array[Byte](Memory.MAX_MEMORY)).writeByte(0x0000, 0xC8.toByte)
      val cpu = new CPU(ram)
      cpu.state = cpu.state.withRegister(Registers.ID.A, 0x42)
      val state = cpu.step
      assert(state.getRegister(Registers.ID.B) == 0x42)
    }
  }
}