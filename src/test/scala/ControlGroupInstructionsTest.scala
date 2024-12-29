package net.mcribbs.s8008

import org.scalatest.*
import org.scalatest.wordspec.*
import spire.implicits.ub
import spire.math.UByte

class ControlGroupInstructionsTest extends AnyWordSpec {

  "HLT" should {
    "set the Halt flag to true for the 0x00 option" in {
      val ram = Memory(new Array[UByte](Memory.MAX_MEMORY)).writeByte(0x0000, UByte(0x00))
      val cpu = new CPU(ram)
      val state = cpu.step
      assert(state.halt)
    }

    "set the Halt flag to true for the 0xFF option" in {
      val ram = Memory(new Array[UByte](Memory.MAX_MEMORY)).writeByte(0x0000, UByte(0xFF))
      val cpu = new CPU(ram)
      val state = cpu.step
      assert(state.halt)
    }
  }
}