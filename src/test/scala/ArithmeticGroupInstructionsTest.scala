package net.mcribbs.s8008

import org.scalatest.*
import org.scalatest.wordspec.*
import spire.math.UByte

class ArithmeticGroupInstructionsTest extends AnyWordSpec {

  "ADM" should {
    "Add the content of memory register M to the accumulator and properly set the flags" in {
      val ram = Memory(new Array[UByte](Memory.MAX_MEMORY)).writeByte(0x0000, UByte(0x87))
        .writeByte(0x0001, UByte(0x05))
      val cpu = new CPU(ram)
      cpu.state = cpu.state.withRegister(Register.A, UByte(0xFF))
        .withRegister(Register.H, UByte(0x00)).withRegister(Register.L, UByte(0x01))
      val state = cpu.step
      assert(state.getRegister(Register.A) == UByte(0x04))
      assert(state.flags.carry)
      assert(!state.flags.zero)
      assert(!state.flags.sign)
      assert(!state.flags.parity)
      // TODO more flag tests
    }
  }

  "ADI" should {
    "Add the content of data B...B to the accumulator and properly set the flags" in {
      val ram = Memory(new Array[UByte](Memory.MAX_MEMORY)).writeByte(0x0000, UByte(0x04))
        .writeByte(0x0001, UByte(0x05))
      val cpu = new CPU(ram)
      cpu.state = cpu.state.withRegister(Register.A, UByte(0xFF))
      val state = cpu.step
      assert(state.getRegister(Register.A) == UByte(0x04))
      assert(state.flags.carry)
      assert(!state.flags.zero)
      assert(!state.flags.sign)
      assert(!state.flags.parity)
      // TODO more flag tests
    }
  }

  "ADr" should {
    "Add the content of index register r and carry flag to the accumulator and properly set the flags" in {
      val ram = Memory(new Array[UByte](Memory.MAX_MEMORY)).writeByte(0x0000, UByte(0x81))
      val cpu = new CPU(ram)
      cpu.state = cpu.state.withRegister(Register.A, UByte(0xFF)).withRegister(Register.B, UByte(0x05))
      val state = cpu.step
      assert(state.getRegister(Register.A) == UByte(0x04))
      assert(state.flags.carry)
      assert(!state.flags.zero)
      assert(!state.flags.sign)
      assert(!state.flags.parity)
      // TODO more flag tests

    }
  }

  "ACM" should {
    "Add the content of memory register M to the accumulator and properly set the flags" in {
      val ram = Memory(new Array[UByte](Memory.MAX_MEMORY)).writeByte(0x0000, UByte(0x8F))
        .writeByte(0x0001, UByte(0x05))
      val cpu = new CPU(ram)
      cpu.state = cpu.state.withRegister(Register.A, UByte(0xFF))
        .withRegister(Register.H, UByte(0x00)).withRegister(Register.L, UByte(0x01))
        .withFlag(Flag.C, true)
      val state = cpu.step
      assert(state.getRegister(Register.A) == UByte(0x05))
      assert(state.flags.carry)
      assert(!state.flags.zero)
      assert(!state.flags.sign)
      assert(state.flags.parity)
      // TODO more flag tests
    }
  }

  "ACI" should {
    "Add the content of data B...B to the accumulator and properly set the flags" in {
      val ram = Memory(new Array[UByte](Memory.MAX_MEMORY)).writeByte(0x0000, UByte(0x04))
        .writeByte(0x0001, UByte(0x05))
      val cpu = new CPU(ram)
      cpu.state = cpu.state.withRegister(Register.A, UByte(0xFF))
      val state = cpu.step
      assert(state.getRegister(Register.A) == UByte(0x04))
      assert(state.flags.carry)
      assert(!state.flags.zero)
      assert(!state.flags.sign)
      assert(!state.flags.parity)
      // TODO more flag tests
    }
  }

  "ACr" should {
    "Add the content of index register r to the accumulator and properly set the flags" in {
      val ram = Memory(new Array[UByte](Memory.MAX_MEMORY)).writeByte(0x0000, UByte(0x81))
      val cpu = new CPU(ram)
      cpu.state = cpu.state.withRegister(Register.A, UByte(0xFF)).withRegister(Register.B, UByte(0x05))
      val state = cpu.step
      assert(state.getRegister(Register.A) == UByte(0x04))
      assert(state.flags.carry)
      assert(!state.flags.zero)
      assert(!state.flags.sign)
      assert(!state.flags.parity)
      // TODO more flag tests

    }
  }

  "SUM" should {
    "Add the content of memory register M to the accumulator and properly set the flags" in {
      val ram = Memory(new Array[UByte](Memory.MAX_MEMORY)).writeByte(0x0000, UByte(0x97))
        .writeByte(0x0001, UByte(0x05))
      val cpu = new CPU(ram)
      cpu.state = cpu.state.withRegister(Register.A, UByte(0xFF))
        .withRegister(Register.H, UByte(0x00)).withRegister(Register.L, UByte(0x01))
      val state = cpu.step
      assert(state.getRegister(Register.A) == UByte(0xFA))
      assert(!state.flags.carry)
      assert(!state.flags.zero)
      assert(state.flags.sign)
      assert(state.flags.parity)
      // TODO more flag tests
    }
  }

  "SUI" should {
    "Add the content of data B...B to the accumulator and properly set the flags" in {
      val ram = Memory(new Array[UByte](Memory.MAX_MEMORY)).writeByte(0x0000, UByte(0x14))
        .writeByte(0x0001, UByte(0x05))
      val cpu = new CPU(ram)
      cpu.state = cpu.state.withRegister(Register.A, UByte(0xFF))
      val state = cpu.step
      assert(state.getRegister(Register.A) == UByte(0xFA))
      assert(!state.flags.carry)
      assert(!state.flags.zero)
      assert(state.flags.sign)
      assert(state.flags.parity)
      // TODO more flag tests
    }
  }

  "SUr" should {
    "Add the content of index register r and carry flag to the accumulator and properly set the flags" in {
      val ram = Memory(new Array[UByte](Memory.MAX_MEMORY)).writeByte(0x0000, UByte(0x91))
      val cpu = new CPU(ram)
      cpu.state = cpu.state.withRegister(Register.A, UByte(0xFF)).withRegister(Register.B, UByte(0x05))
      val state = cpu.step
      assert(state.getRegister(Register.A) == UByte(0xFA))
      assert(!state.flags.carry)
      assert(!state.flags.zero)
      assert(state.flags.sign)
      assert(state.flags.parity)
      // TODO more flag tests

    }
  }

  "SBM" should {
    "Add the content of memory register M to the accumulator and properly set the flags" in {
      val ram = Memory(new Array[UByte](Memory.MAX_MEMORY)).writeByte(0x0000, UByte(0x9F))
        .writeByte(0x0001, UByte(0x05))
      val cpu = new CPU(ram)
      cpu.state = cpu.state.withRegister(Register.A, UByte(0xFF))
        .withRegister(Register.H, UByte(0x00)).withRegister(Register.L, UByte(0x01))
        .withFlag(Flag.C, true)
      val state = cpu.step
      assert(state.getRegister(Register.A) == UByte(0xF9))
      assert(!state.flags.carry)
      assert(!state.flags.zero)
      assert(state.flags.sign)
      assert(state.flags.parity)
      // TODO more flag tests
    }
  }

  "SBI" should {
    "Add the content of data B...B to the accumulator and properly set the flags" in {
      val ram = Memory(new Array[UByte](Memory.MAX_MEMORY)).writeByte(0x0000, UByte(0x1C))
        .writeByte(0x0001, UByte(0x05))
      val cpu = new CPU(ram)
      cpu.state = cpu.state.withRegister(Register.A, UByte(0xFF))
        .withFlag(Flag.C, true)
      val state = cpu.step
      assert(state.getRegister(Register.A) == UByte(0xF9))
      assert(!state.flags.carry)
      assert(!state.flags.zero)
      assert(state.flags.sign)
      assert(state.flags.parity)
      // TODO more flag tests
    }
  }

  "SBr" should {
    "Add the content of index register r and carry flag to the accumulator and properly set the flags" in {
      val ram = Memory(new Array[UByte](Memory.MAX_MEMORY)).writeByte(0x0000, UByte(0x99))
      val cpu = new CPU(ram)
      cpu.state = cpu.state.withRegister(Register.A, UByte(0xFF)).withRegister(Register.B, UByte(0x05))
        .withFlag(Flag.C, true)
      val state = cpu.step
      assert(state.getRegister(Register.A) == UByte(0xF9))
      assert(!state.flags.carry)
      assert(!state.flags.zero)
      assert(state.flags.sign)
      assert(state.flags.parity)
      // TODO more flag tests

    }
  }

  "NDM" should {
    "Add the content of memory register M to the accumulator and properly set the flags" in {
      val ram = Memory(new Array[UByte](Memory.MAX_MEMORY)).writeByte(0x0000, UByte(0xA7))
        .writeByte(0x0001, UByte(0x0D))
      val cpu = new CPU(ram)
      cpu.state = cpu.state.withRegister(Register.A, UByte(0x3C))
        .withRegister(Register.H, UByte(0x00)).withRegister(Register.L, UByte(0x01))
      val state = cpu.step
      assert(state.getRegister(Register.A) == UByte(0x0C))
      assert(!state.flags.carry)
      assert(!state.flags.zero)
      assert(!state.flags.sign)
      assert(state.flags.parity)
      // TODO more flag tests
    }
  }

  "NDI" should {
    "Add the content of data B...B to the accumulator and properly set the flags" in {
      val ram = Memory(new Array[UByte](Memory.MAX_MEMORY)).writeByte(0x0000, UByte(0x24))
        .writeByte(0x0001, UByte(0x0D))
      val cpu = new CPU(ram)
      cpu.state = cpu.state.withRegister(Register.A, UByte(0x3C))
        .withFlag(Flag.C, true)
      val state = cpu.step
      assert(state.getRegister(Register.A) == UByte(0x0C))
      assert(!state.flags.carry)
      assert(!state.flags.zero)
      assert(!state.flags.sign)
      assert(state.flags.parity)
      // TODO more flag tests
    }
  }

  "NDr" should {
    "Add the content of index register r and carry flag to the accumulator and properly set the flags" in {
      val ram = Memory(new Array[UByte](Memory.MAX_MEMORY)).writeByte(0x0000, UByte(0xA1))
      val cpu = new CPU(ram)
      cpu.state = cpu.state.withRegister(Register.A, UByte(0x3C)).withRegister(Register.B, UByte(0x0D))
        .withFlag(Flag.C, true)
      val state = cpu.step
      assert(state.getRegister(Register.A) == UByte(0x0C))
      assert(!state.flags.carry)
      assert(!state.flags.zero)
      assert(!state.flags.sign)
      assert(state.flags.parity)
      // TODO more flag tests

    }
  }

  "XRM" should {
    "Add the content of memory register M to the accumulator and properly set the flags" in {
      val ram = Memory(new Array[UByte](Memory.MAX_MEMORY)).writeByte(0x0000, UByte(0xAF))
        .writeByte(0x0001, UByte(0x0D))
      val cpu = new CPU(ram)
      cpu.state = cpu.state.withRegister(Register.A, UByte(0x3C))
        .withRegister(Register.H, UByte(0x00)).withRegister(Register.L, UByte(0x01))
      val state = cpu.step
      assert(state.getRegister(Register.A) == UByte(0x31))
      assert(!state.flags.carry)
      assert(!state.flags.zero)
      assert(!state.flags.sign)
      assert(!state.flags.parity)
      // TODO more flag tests
    }
  }

  "XRI" should {
    "Add the content of data B...B to the accumulator and properly set the flags" in {
      val ram = Memory(new Array[UByte](Memory.MAX_MEMORY)).writeByte(0x0000, UByte(0x2C))
        .writeByte(0x0001, UByte(0x0D))
      val cpu = new CPU(ram)
      cpu.state = cpu.state.withRegister(Register.A, UByte(0x3C))
        .withFlag(Flag.C, true)
      val state = cpu.step
      assert(state.getRegister(Register.A) == UByte(0x31))
      assert(!state.flags.carry)
      assert(!state.flags.zero)
      assert(!state.flags.sign)
      assert(!state.flags.parity)
      // TODO more flag tests
    }
  }

  "XRr" should {
    "Add the content of index register r and carry flag to the accumulator and properly set the flags" in {
      val ram = Memory(new Array[UByte](Memory.MAX_MEMORY)).writeByte(0x0000, UByte(0xA9))
      val cpu = new CPU(ram)
      cpu.state = cpu.state.withRegister(Register.A, UByte(0x3C)).withRegister(Register.B, UByte(0x0D))
        .withFlag(Flag.C, true)
      val state = cpu.step
      assert(state.getRegister(Register.A) == UByte(0x31))
      assert(!state.flags.carry)
      assert(!state.flags.zero)
      assert(!state.flags.sign)
      assert(!state.flags.parity)
      // TODO more flag tests

    }
  }

  "ORM" should {
    "Add the content of memory register M to the accumulator and properly set the flags" in {
      val ram = Memory(new Array[UByte](Memory.MAX_MEMORY)).writeByte(0x0000, UByte(0xB7))
        .writeByte(0x0001, UByte(0x0D))
      val cpu = new CPU(ram)
      cpu.state = cpu.state.withRegister(Register.A, UByte(0x3C))
        .withRegister(Register.H, UByte(0x00)).withRegister(Register.L, UByte(0x01))
      val state = cpu.step
      assert(state.getRegister(Register.A) == UByte(0x3D))
      assert(!state.flags.carry)
      assert(!state.flags.zero)
      assert(!state.flags.sign)
      assert(!state.flags.parity)
      // TODO more flag tests
    }
  }

  "ORI" should {
    "Add the content of data B...B to the accumulator and properly set the flags" in {
      val ram = Memory(new Array[UByte](Memory.MAX_MEMORY)).writeByte(0x0000, UByte(0x34))
        .writeByte(0x0001, UByte(0x0D))
      val cpu = new CPU(ram)
      cpu.state = cpu.state.withRegister(Register.A, UByte(0x3C))
        .withFlag(Flag.C, true)
      val state = cpu.step
      assert(state.getRegister(Register.A) == UByte(0x3D))
      assert(!state.flags.carry)
      assert(!state.flags.zero)
      assert(!state.flags.sign)
      assert(!state.flags.parity)
      // TODO more flag tests
    }
  }

  "ORr" should {
    "Add the content of index register r and carry flag to the accumulator and properly set the flags" in {
      val ram = Memory(new Array[UByte](Memory.MAX_MEMORY)).writeByte(0x0000, UByte(0xB1))
      val cpu = new CPU(ram)
      cpu.state = cpu.state.withRegister(Register.A, UByte(0x3C)).withRegister(Register.B, UByte(0x0D))
        .withFlag(Flag.C, true)
      val state = cpu.step
      assert(state.getRegister(Register.A) == UByte(0x3D))
      assert(!state.flags.carry)
      assert(!state.flags.zero)
      assert(!state.flags.sign)
      assert(!state.flags.parity)
      // TODO more flag tests

    }
  }

  "INr" should {
    "increment the contents of register r and set ZSP" in {
      val ram = Memory(new Array[UByte](Memory.MAX_MEMORY)).writeByte(0x0000, UByte(0x08))
      val cpu = new CPU(ram)
      cpu.state = cpu.state.withRegister(Register.B, UByte(0xFD))
      cpu.state = cpu.step
      assert(cpu.state.getRegister(Register.B) == UByte(0xFE))
      val flags = Flags.calcZSP(cpu.state.getRegister(Register.B))
      assert(!flags.zero)
      assert(flags.sign)
      assert(!flags.parity)

      cpu.state = cpu.state.decrementSP.withRegister(Register.B, UByte(0xFF))
      cpu.state = cpu.step
      assert(cpu.state.getRegister(Register.B) == UByte(0x00))
      val flags2 = Flags.calcZSP(cpu.state.getRegister(Register.B))
      assert(flags2.zero)
      assert(!flags2.sign)
      assert(flags2.parity)
    }
  }

  "DCr" should {
    "decrement the contents of register r" in {
      val ram = Memory(new Array[UByte](Memory.MAX_MEMORY)).writeByte(0x0000, UByte(0x09))
      val cpu = new CPU(ram)
      cpu.state = cpu.state.withRegister(Register.B, UByte(0xFF))
      cpu.state = cpu.step
      assert(cpu.state.getRegister(Register.B) == UByte(0xFE))
      val flags = Flags.calcZSP(cpu.state.getRegister(Register.B))
      assert(!flags.zero)
      assert(flags.sign)
      assert(!flags.parity)

      cpu.state = cpu.state.decrementSP.withRegister(Register.B, UByte(0x01))
      cpu.state = cpu.step
      assert(cpu.state.getRegister(Register.B) == UByte(0x00))
      val flags2 = Flags.calcZSP(cpu.state.getRegister(Register.B))
      assert(flags2.zero)
      assert(!flags2.sign)
      assert(flags2.parity)
    }
  }
}