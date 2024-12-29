package net.mcribbs.s8008

import org.scalatest.*
import org.scalatest.wordspec.*
import spire.math.UByte

class CallAndReturnGroupInstructionsTest extends AnyWordSpec {

  "CAL" should {
    "push the memory address B3...B3B2...B2 onto the stack" in {
      val ram = Memory(new Array[UByte](Memory.MAX_MEMORY)).writeByte(0x0000, UByte(0x46))
        .writeByte(0x0001, UByte(0x04)).writeByte(0x0002, UByte(0x02))
      val cpu = new CPU(ram)
      val state = cpu.step
      assert(state.PC == 0x0204)
    }
  }

  "CFC" should {
    "set the PC (unconditionally jump) to memory address B3...B3B2...B2 if C is false" in {
      val ram = Memory(new Array[UByte](Memory.MAX_MEMORY)).writeByte(0x0000, UByte(0x42))
        .writeByte(0x0001, UByte(0x04)).writeByte(0x0002, UByte(0x02))
      val cpu = new CPU(ram)
      cpu.state = cpu.state.copy(flags = cpu.state.flags.copy(carry = false))
      val state = cpu.step
      assert(state.PC == 0x0204)
    }

    "but not if C is true" in {
      val ram = Memory(new Array[UByte](Memory.MAX_MEMORY)).writeByte(0x0000, UByte(0x42))
        .writeByte(0x0001, UByte(0x04)).writeByte(0x0002, UByte(0x02))
      val cpu = new CPU(ram)
      cpu.state = cpu.state.copy(flags = cpu.state.flags.copy(carry = true))
      val state = cpu.step
      assert(state.PC == 0x0003)
    }
  }

  "CFZ" should {
    "set the PC (unconditionally jump) to memory address B3...B3B2...B2 if Z is false" in {
      val ram = Memory(new Array[UByte](Memory.MAX_MEMORY)).writeByte(0x0000, UByte(0x4A))
        .writeByte(0x0001, UByte(0x04)).writeByte(0x0002, UByte(0x02))
      val cpu = new CPU(ram)
      cpu.state = cpu.state.copy(flags = cpu.state.flags.copy(zero = false))
      val state = cpu.step
      assert(state.PC == 0x0204)
    }

    "but not if Z is true" in {
      val ram = Memory(new Array[UByte](Memory.MAX_MEMORY)).writeByte(0x0000, UByte(0x4A))
        .writeByte(0x0001, UByte(0x04)).writeByte(0x0002, UByte(0x02))
      val cpu = new CPU(ram)
      cpu.state = cpu.state.copy(flags = cpu.state.flags.copy(zero = true))
      val state = cpu.step
      assert(state.PC == 0x0003)
    }
  }

  "CFS" should {
    "set the PC (unconditionally jump) to memory address B3...B3B2...B2 if S is false" in {
      val ram = Memory(new Array[UByte](Memory.MAX_MEMORY)).writeByte(0x0000, UByte(0x52))
        .writeByte(0x0001, UByte(0x04)).writeByte(0x0002, UByte(0x02))
      val cpu = new CPU(ram)
      cpu.state = cpu.state.copy(flags = cpu.state.flags.copy(sign = false))
      val state = cpu.step
      assert(state.PC == 0x0204)
    }

    "but not if S is true" in {
      val ram = Memory(new Array[UByte](Memory.MAX_MEMORY)).writeByte(0x0000, UByte(0x52))
        .writeByte(0x0001, UByte(0x04)).writeByte(0x0002, UByte(0x02))
      val cpu = new CPU(ram)
      cpu.state = cpu.state.copy(flags = cpu.state.flags.copy(sign = true))
      val state = cpu.step
      assert(state.PC == 0x0003)
    }
  }

  "CFP" should {
    "set the PC (unconditionally jump) to memory address B3...B3B2...B2 if P is false" in {
      val ram = Memory(new Array[UByte](Memory.MAX_MEMORY)).writeByte(0x0000, UByte(0x5A))
        .writeByte(0x0001, UByte(0x04)).writeByte(0x0002, UByte(0x02))
      val cpu = new CPU(ram)
      cpu.state = cpu.state.copy(flags = cpu.state.flags.copy(parity = false))
      val state = cpu.step
      assert(state.PC == 0x0204)
    }

    "but not if P is true" in {
      val ram = Memory(new Array[UByte](Memory.MAX_MEMORY)).writeByte(0x0000, UByte(0x5A))
        .writeByte(0x0001, UByte(0x04)).writeByte(0x0002, UByte(0x02))
      val cpu = new CPU(ram)
      cpu.state = cpu.state.copy(flags = cpu.state.flags.copy(parity = true))
      val state = cpu.step
      assert(state.PC == 0x0003)
    }
  }

  "CC" should {
    "set the PC (unconditionally jump) to memory address B3...B3B2...B2 if C is true" in {
      val ram = Memory(new Array[UByte](Memory.MAX_MEMORY)).writeByte(0x0000, UByte(0x62))
        .writeByte(0x0001, UByte(0x04)).writeByte(0x0002, UByte(0x02))
      val cpu = new CPU(ram)
      cpu.state = cpu.state.copy(flags = cpu.state.flags.copy(carry = true))
      val state = cpu.step
      assert(state.PC == 0x0204)
    }

    "but not if C is false" in {
      val ram = Memory(new Array[UByte](Memory.MAX_MEMORY)).writeByte(0x0000, UByte(0x62))
        .writeByte(0x0001, UByte(0x04)).writeByte(0x0002, UByte(0x02))
      val cpu = new CPU(ram)
      cpu.state = cpu.state.copy(flags = cpu.state.flags.copy(carry = false))
      val state = cpu.step
      assert(state.PC == 0x0003)
    }
  }

  "CZ" should {
    "set the PC (unconditionally jump) to memory address B3...B3B2...B2 if Z is true" in {
      val ram = Memory(new Array[UByte](Memory.MAX_MEMORY)).writeByte(0x0000, UByte(0x6A))
        .writeByte(0x0001, UByte(0x04)).writeByte(0x0002, UByte(0x02))
      val cpu = new CPU(ram)
      cpu.state = cpu.state.copy(flags = cpu.state.flags.copy(zero = true))
      val state = cpu.step
      assert(state.PC == 0x0204)
    }

    "but not if Z is false" in {
      val ram = Memory(new Array[UByte](Memory.MAX_MEMORY)).writeByte(0x0000, UByte(0x6A))
        .writeByte(0x0001, UByte(0x04)).writeByte(0x0002, UByte(0x02))
      val cpu = new CPU(ram)
      cpu.state = cpu.state.copy(flags = cpu.state.flags.copy(zero = false))
      val state = cpu.step
      assert(state.PC == 0x0003)
    }
  }

  "CS" should {
    "set the PC (unconditionally jump) to memory address B3...B3B2...B2 if S is true" in {
      val ram = Memory(new Array[UByte](Memory.MAX_MEMORY)).writeByte(0x0000, UByte(0x72))
        .writeByte(0x0001, UByte(0x04)).writeByte(0x0002, UByte(0x02))
      val cpu = new CPU(ram)
      cpu.state = cpu.state.copy(flags = cpu.state.flags.copy(sign = true))
      val state = cpu.step
      assert(state.PC == 0x0204)
    }

    "but not if S is false" in {
      val ram = Memory(new Array[UByte](Memory.MAX_MEMORY)).writeByte(0x0000, UByte(0x72))
        .writeByte(0x0001, UByte(0x04)).writeByte(0x0002, UByte(0x02))
      val cpu = new CPU(ram)
      cpu.state = cpu.state.copy(flags = cpu.state.flags.copy(sign = false))
      val state = cpu.step
      assert(state.PC == 0x0003)
    }
  }

  "CP" should {
    "set the PC (unconditionally jump) to memory address B3...B3B2...B2 if P is true" in {
      val ram = Memory(new Array[UByte](Memory.MAX_MEMORY)).writeByte(0x0000, UByte(0x7A))
        .writeByte(0x0001, UByte(0x04)).writeByte(0x0002, UByte(0x02))
      val cpu = new CPU(ram)
      cpu.state = cpu.state.copy(flags = cpu.state.flags.copy(parity = true))
      val state = cpu.step
      assert(state.PC == 0x0204)
    }

    "but not if P is false" in {
      val ram = Memory(new Array[UByte](Memory.MAX_MEMORY)).writeByte(0x0000, UByte(0x7A))
        .writeByte(0x0001, UByte(0x04)).writeByte(0x0002, UByte(0x02))
      val cpu = new CPU(ram)
      cpu.state = cpu.state.copy(flags = cpu.state.flags.copy(parity = false))
      val state = cpu.step
      assert(state.PC == 0x0003)
    }
  }

  "RET" should {
    "decrement the stack pointer, restoring the previous address" in {
      val ram = Memory(new Array[UByte](Memory.MAX_MEMORY)).writeByte(0x0000, UByte(0x46))
        .writeByte(0x0001, UByte(0x04)).writeByte(0x0002, UByte(0x02)).writeByte(0x0204, UByte(0x07))
      val cpu = new CPU(ram)
      cpu.state = cpu.step
      assert(cpu.state.PC == 0x0204)
      cpu.state = cpu.step
      assert(cpu.state.PC == 0x0003)
    }
  }

  "RFC" should {
    "decrement the stack pointer, restoring the previous address if C is false" in {
      val ram = Memory(new Array[UByte](Memory.MAX_MEMORY)).writeByte(0x0000, UByte(0x46))
        .writeByte(0x0001, UByte(0x04)).writeByte(0x0002, UByte(0x02)).writeByte(0x0204, UByte(0x03))
      val cpu = new CPU(ram)
      cpu.state = cpu.step
      assert(cpu.state.PC == 0x0204)
      cpu.state = cpu.state.copy(flags = cpu.state.flags.copy(carry = false))
      cpu.state = cpu.step
      assert(cpu.state.PC == 0x0003)
    }

    "but not if C is true" in {
      val ram = Memory(new Array[UByte](Memory.MAX_MEMORY)).writeByte(0x0000, UByte(0x46))
        .writeByte(0x0001, UByte(0x04)).writeByte(0x0002, UByte(0x02)).writeByte(0x0204, UByte(0x03))
      val cpu = new CPU(ram)
      cpu.state = cpu.step
      assert(cpu.state.PC == 0x0204)
      cpu.state = cpu.state.copy(flags = cpu.state.flags.copy(carry = true))
      val state = cpu.step
      assert(state.PC == 0x0205)
    }
  }

  "RFZ" should {
    "decrement the stack pointer, restoring the previous address if Z is false" in {
      val ram = Memory(new Array[UByte](Memory.MAX_MEMORY)).writeByte(0x0000, UByte(0x46))
        .writeByte(0x0001, UByte(0x04)).writeByte(0x0002, UByte(0x02)).writeByte(0x0204, UByte(0x0B))
      val cpu = new CPU(ram)
      cpu.state = cpu.step
      assert(cpu.state.PC == 0x0204)
      cpu.state = cpu.state.copy(flags = cpu.state.flags.copy(zero = false))
      cpu.state = cpu.step
      assert(cpu.state.PC == 0x0003)
    }

    "but not if Z is true" in {
      val ram = Memory(new Array[UByte](Memory.MAX_MEMORY)).writeByte(0x0000, UByte(0x46))
        .writeByte(0x0001, UByte(0x04)).writeByte(0x0002, UByte(0x02)).writeByte(0x0204, UByte(0x0B))
      val cpu = new CPU(ram)
      cpu.state = cpu.step
      assert(cpu.state.PC == 0x0204)
      cpu.state = cpu.state.copy(flags = cpu.state.flags.copy(zero = true))
      val state = cpu.step
      assert(state.PC == 0x0205)
    }
  }

  "RFS" should {
    "decrement the stack pointer, restoring the previous address if S is false" in {
      val ram = Memory(new Array[UByte](Memory.MAX_MEMORY)).writeByte(0x0000, UByte(0x46))
        .writeByte(0x0001, UByte(0x04)).writeByte(0x0002, UByte(0x02)).writeByte(0x0204, UByte(0x13))
      val cpu = new CPU(ram)
      cpu.state = cpu.step
      assert(cpu.state.PC == 0x0204)
      cpu.state = cpu.state.copy(flags = cpu.state.flags.copy(sign = false))
      cpu.state = cpu.step
      assert(cpu.state.PC == 0x0003)
    }

    "but not if S is true" in {
      val ram = Memory(new Array[UByte](Memory.MAX_MEMORY)).writeByte(0x0000, UByte(0x46))
        .writeByte(0x0001, UByte(0x04)).writeByte(0x0002, UByte(0x02)).writeByte(0x0204, UByte(0x13))
      val cpu = new CPU(ram)
      cpu.state = cpu.step
      assert(cpu.state.PC == 0x0204)
      cpu.state = cpu.state.copy(flags = cpu.state.flags.copy(sign = true))
      val state = cpu.step
      assert(state.PC == 0x0205)
    }
  }

  "RFP" should {
    "decrement the stack pointer, restoring the previous address if P is false" in {
      val ram = Memory(new Array[UByte](Memory.MAX_MEMORY)).writeByte(0x0000, UByte(0x46))
        .writeByte(0x0001, UByte(0x04)).writeByte(0x0002, UByte(0x02)).writeByte(0x0204, UByte(0x1B))
      val cpu = new CPU(ram)
      cpu.state = cpu.step
      assert(cpu.state.PC == 0x0204)
      cpu.state = cpu.state.copy(flags = cpu.state.flags.copy(parity = false))
      cpu.state = cpu.step
      assert(cpu.state.PC == 0x0003)
    }

    "but not if S is true" in {
      val ram = Memory(new Array[UByte](Memory.MAX_MEMORY)).writeByte(0x0000, UByte(0x46))
        .writeByte(0x0001, UByte(0x04)).writeByte(0x0002, UByte(0x02)).writeByte(0x0204, UByte(0x1B))
      val cpu = new CPU(ram)
      cpu.state = cpu.step
      assert(cpu.state.PC == 0x0204)
      cpu.state = cpu.state.copy(flags = cpu.state.flags.copy(parity = true))
      val state = cpu.step
      assert(state.PC == 0x0205)
    }
  }

  "RC" should {
    "decrement the stack pointer, restoring the previous address if C is true" in {
      val ram = Memory(new Array[UByte](Memory.MAX_MEMORY)).writeByte(0x0000, UByte(0x46))
        .writeByte(0x0001, UByte(0x04)).writeByte(0x0002, UByte(0x02)).writeByte(0x0204, UByte(0x23))
      val cpu = new CPU(ram)
      cpu.state = cpu.step
      assert(cpu.state.PC == 0x0204)
      cpu.state = cpu.state.copy(flags = cpu.state.flags.copy(carry = true))
      cpu.state = cpu.step
      assert(cpu.state.PC == 0x0003)
    }

    "but not if C is false" in {
      val ram = Memory(new Array[UByte](Memory.MAX_MEMORY)).writeByte(0x0000, UByte(0x46))
        .writeByte(0x0001, UByte(0x04)).writeByte(0x0002, UByte(0x02)).writeByte(0x0204, UByte(0x23))
      val cpu = new CPU(ram)
      cpu.state = cpu.step
      assert(cpu.state.PC == 0x0204)
      cpu.state = cpu.state.copy(flags = cpu.state.flags.copy(carry = false))
      val state = cpu.step
      assert(state.PC == 0x0205)
    }
  }

  "RZ" should {
    "decrement the stack pointer, restoring the previous address if Z is true" in {
      val ram = Memory(new Array[UByte](Memory.MAX_MEMORY)).writeByte(0x0000, UByte(0x46))
        .writeByte(0x0001, UByte(0x04)).writeByte(0x0002, UByte(0x02)).writeByte(0x0204, UByte(0x2B))
      val cpu = new CPU(ram)
      cpu.state = cpu.step
      assert(cpu.state.PC == 0x0204)
      cpu.state = cpu.state.copy(flags = cpu.state.flags.copy(zero = true))
      cpu.state = cpu.step
      assert(cpu.state.PC == 0x0003)
    }

    "but not if Z is false" in {
      val ram = Memory(new Array[UByte](Memory.MAX_MEMORY)).writeByte(0x0000, UByte(0x46))
        .writeByte(0x0001, UByte(0x04)).writeByte(0x0002, UByte(0x02)).writeByte(0x0204, UByte(0x2B))
      val cpu = new CPU(ram)
      cpu.state = cpu.step
      assert(cpu.state.PC == 0x0204)
      cpu.state = cpu.state.copy(flags = cpu.state.flags.copy(zero = false))
      val state = cpu.step
      assert(state.PC == 0x0205)
    }
  }

  "RS" should {
    "decrement the stack pointer, restoring the previous address if S is true" in {
      val ram = Memory(new Array[UByte](Memory.MAX_MEMORY)).writeByte(0x0000, UByte(0x46))
        .writeByte(0x0001, UByte(0x04)).writeByte(0x0002, UByte(0x02)).writeByte(0x0204, UByte(0x33))
      val cpu = new CPU(ram)
      cpu.state = cpu.step
      assert(cpu.state.PC == 0x0204)
      cpu.state = cpu.state.copy(flags = cpu.state.flags.copy(sign = true))
      cpu.state = cpu.step
      assert(cpu.state.PC == 0x0003)
    }

    "but not if S is false" in {
      val ram = Memory(new Array[UByte](Memory.MAX_MEMORY)).writeByte(0x0000, UByte(0x46))
        .writeByte(0x0001, UByte(0x04)).writeByte(0x0002, UByte(0x02)).writeByte(0x0204, UByte(0x33))
      val cpu = new CPU(ram)
      cpu.state = cpu.step
      assert(cpu.state.PC == 0x0204)
      cpu.state = cpu.state.copy(flags = cpu.state.flags.copy(sign = false))
      val state = cpu.step
      assert(state.PC == 0x0205)
    }
  }

  "RP" should {
    "decrement the stack pointer, restoring the previous address if P is true" in {
      val ram = Memory(new Array[UByte](Memory.MAX_MEMORY)).writeByte(0x0000, UByte(0x46))
        .writeByte(0x0001, UByte(0x04)).writeByte(0x0002, UByte(0x02)).writeByte(0x0204, UByte(0x3B))
      val cpu = new CPU(ram)
      cpu.state = cpu.step
      assert(cpu.state.PC == 0x0204)
      cpu.state = cpu.state.copy(flags = cpu.state.flags.copy(parity = true))
      cpu.state = cpu.step
      assert(cpu.state.PC == 0x0003)
    }

    "but not if P is false" in {
      val ram = Memory(new Array[UByte](Memory.MAX_MEMORY)).writeByte(0x0000, UByte(0x46))
        .writeByte(0x0001, UByte(0x04)).writeByte(0x0002, UByte(0x02)).writeByte(0x0204, UByte(0x3B))
      val cpu = new CPU(ram)
      cpu.state = cpu.step
      assert(cpu.state.PC == 0x0204)
      cpu.state = cpu.state.copy(flags = cpu.state.flags.copy(parity = false))
      val state = cpu.step
      assert(state.PC == 0x0205)
    }
  }

  "RST" should {
    "Call the subroutine at memory address AAAOOO and move the SP up one level in the stack" in {
      val ram = Memory(new Array[UByte](Memory.MAX_MEMORY)).writeByte(0x0000, UByte(0x15))
        .writeByte(0x0010, UByte(0xFF))
      val cpu = new CPU(ram)
      cpu.state = cpu.step
      assert(cpu.state.PC == 0x0010)
    }
  }
}