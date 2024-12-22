package net.mcribbs.s8008

import org.scalatest.*
import org.scalatest.wordspec.*

class JumpGroupInstructionsTest extends AnyWordSpec {

  "JMP" should {
    "set the PC (unconditionally jump) to memory address B3...B3B2...B2" in {
      val ram = Memory(new Array[Byte](Memory.MAX_MEMORY)).writeByte(0x0000, 0x44)
        .writeByte(0x0001, 0x04).writeByte(0x0002, 0x02)
      val cpu = new CPU(ram)
      val state = cpu.step
      assert(state.PC == 0x0204)
    }
  }

  "JFC" should {
    "set the PC (unconditionally jump) to memory address B3...B3B2...B2 if C is false" in {
      val ram = Memory(new Array[Byte](Memory.MAX_MEMORY)).writeByte(0x0000, 0x40)
        .writeByte(0x0001, 0x04).writeByte(0x0002, 0x02)
      val cpu = new CPU(ram)
      cpu.state = cpu.state.copy(flags = cpu.state.flags.copy(carry = false))
      val state = cpu.step
      assert(state.PC == 0x0204)
    }

    "but not if C is true" in {
      val ram = Memory(new Array[Byte](Memory.MAX_MEMORY)).writeByte(0x0000, 0x40)
        .writeByte(0x0001, 0x04).writeByte(0x0002, 0x02)
      val cpu = new CPU(ram)
      cpu.state = cpu.state.copy(flags = cpu.state.flags.copy(carry = true))
      val state = cpu.step
      assert(state.PC == 0x0003)
    }
  }

  "JFZ" should {
    "set the PC (unconditionally jump) to memory address B3...B3B2...B2 if Z is false" in {
      val ram = Memory(new Array[Byte](Memory.MAX_MEMORY)).writeByte(0x0000, 0x48)
        .writeByte(0x0001, 0x04).writeByte(0x0002, 0x02)
      val cpu = new CPU(ram)
      cpu.state = cpu.state.copy(flags = cpu.state.flags.copy(zero = false))
      val state = cpu.step
      assert(state.PC == 0x0204)
    }

    "but not if Z is true" in {
      val ram = Memory(new Array[Byte](Memory.MAX_MEMORY)).writeByte(0x0000, 0x48)
        .writeByte(0x0001, 0x04).writeByte(0x0002, 0x02)
      val cpu = new CPU(ram)
      cpu.state = cpu.state.copy(flags = cpu.state.flags.copy(zero = true))
      val state = cpu.step
      assert(state.PC == 0x0003)
    }
  }

  "JFS" should {
    "set the PC (unconditionally jump) to memory address B3...B3B2...B2 if S is false" in {
      val ram = Memory(new Array[Byte](Memory.MAX_MEMORY)).writeByte(0x0000, 0x50)
        .writeByte(0x0001, 0x04).writeByte(0x0002, 0x02)
      val cpu = new CPU(ram)
      cpu.state = cpu.state.copy(flags = cpu.state.flags.copy(sign = false))
      val state = cpu.step
      assert(state.PC == 0x0204)
    }

    "but not if S is true" in {
      val ram = Memory(new Array[Byte](Memory.MAX_MEMORY)).writeByte(0x0000, 0x50)
        .writeByte(0x0001, 0x04).writeByte(0x0002, 0x02)
      val cpu = new CPU(ram)
      cpu.state = cpu.state.copy(flags = cpu.state.flags.copy(sign = true))
      val state = cpu.step
      assert(state.PC == 0x0003)
    }
  }

  "JFP" should {
    "set the PC (unconditionally jump) to memory address B3...B3B2...B2 if P is false" in {
      val ram = Memory(new Array[Byte](Memory.MAX_MEMORY)).writeByte(0x0000, 0x58)
        .writeByte(0x0001, 0x04).writeByte(0x0002, 0x02)
      val cpu = new CPU(ram)
      cpu.state = cpu.state.copy(flags = cpu.state.flags.copy(parity = false))
      val state = cpu.step
      assert(state.PC == 0x0204)
    }

    "but not if P is true" in {
      val ram = Memory(new Array[Byte](Memory.MAX_MEMORY)).writeByte(0x0000, 0x58)
        .writeByte(0x0001, 0x04).writeByte(0x0002, 0x02)
      val cpu = new CPU(ram)
      cpu.state = cpu.state.copy(flags = cpu.state.flags.copy(parity = true))
      val state = cpu.step
      assert(state.PC == 0x0003)
    }
  }

  "JC" should {
    "set the PC (unconditionally jump) to memory address B3...B3B2...B2 if C is true" in {
      val ram = Memory(new Array[Byte](Memory.MAX_MEMORY)).writeByte(0x0000, 0x60)
        .writeByte(0x0001, 0x04).writeByte(0x0002, 0x02)
      val cpu = new CPU(ram)
      cpu.state = cpu.state.copy(flags = cpu.state.flags.copy(carry = true))
      val state = cpu.step
      assert(state.PC == 0x0204)
    }

    "but not if C is false" in {
      val ram = Memory(new Array[Byte](Memory.MAX_MEMORY)).writeByte(0x0000, 0x60)
        .writeByte(0x0001, 0x04).writeByte(0x0002, 0x02)
      val cpu = new CPU(ram)
      cpu.state = cpu.state.copy(flags = cpu.state.flags.copy(carry = false))
      val state = cpu.step
      assert(state.PC == 0x0003)
    }
  }

  "JZ" should {
    "set the PC (unconditionally jump) to memory address B3...B3B2...B2 if Z is true" in {
      val ram = Memory(new Array[Byte](Memory.MAX_MEMORY)).writeByte(0x0000, 0x68)
        .writeByte(0x0001, 0x04).writeByte(0x0002, 0x02)
      val cpu = new CPU(ram)
      cpu.state = cpu.state.copy(flags = cpu.state.flags.copy(zero = true))
      val state = cpu.step
      assert(state.PC == 0x0204)
    }

    "but not if Z is false" in {
      val ram = Memory(new Array[Byte](Memory.MAX_MEMORY)).writeByte(0x0000, 0x68)
        .writeByte(0x0001, 0x04).writeByte(0x0002, 0x02)
      val cpu = new CPU(ram)
      cpu.state = cpu.state.copy(flags = cpu.state.flags.copy(zero = false))
      val state = cpu.step
      assert(state.PC == 0x0003)
    }
  }

  "JS" should {
    "set the PC (unconditionally jump) to memory address B3...B3B2...B2 if S is true" in {
      val ram = Memory(new Array[Byte](Memory.MAX_MEMORY)).writeByte(0x0000, 0x70)
        .writeByte(0x0001, 0x04).writeByte(0x0002, 0x02)
      val cpu = new CPU(ram)
      cpu.state = cpu.state.copy(flags = cpu.state.flags.copy(sign = true))
      val state = cpu.step
      assert(state.PC == 0x0204)
    }

    "but not if S is false" in {
      val ram = Memory(new Array[Byte](Memory.MAX_MEMORY)).writeByte(0x0000, 0x70)
        .writeByte(0x0001, 0x04).writeByte(0x0002, 0x02)
      val cpu = new CPU(ram)
      cpu.state = cpu.state.copy(flags = cpu.state.flags.copy(sign = false))
      val state = cpu.step
      assert(state.PC == 0x0003)
    }
  }

  "JP" should {
    "set the PC (unconditionally jump) to memory address B3...B3B2...B2 if P is true" in {
      val ram = Memory(new Array[Byte](Memory.MAX_MEMORY)).writeByte(0x0000, 0x78)
        .writeByte(0x0001, 0x04).writeByte(0x0002, 0x02)
      val cpu = new CPU(ram)
      cpu.state = cpu.state.copy(flags = cpu.state.flags.copy(parity = true))
      val state = cpu.step
      assert(state.PC == 0x0204)
    }

    "but not if P is false" in {
      val ram = Memory(new Array[Byte](Memory.MAX_MEMORY)).writeByte(0x0000, 0x78)
        .writeByte(0x0001, 0x04).writeByte(0x0002, 0x02)
      val cpu = new CPU(ram)
      cpu.state = cpu.state.copy(flags = cpu.state.flags.copy(parity = false))
      val state = cpu.step
      assert(state.PC == 0x0003)
    }
  }
}