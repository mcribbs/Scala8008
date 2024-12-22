package net.mcribbs.s8008

import org.scalatest._
import wordspec._

class ArithmeticGroupInstructionsTest extends AnyWordSpec {

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
}