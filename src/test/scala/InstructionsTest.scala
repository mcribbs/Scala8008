package net.mcribbs.s8008

import org.scalatest._
import flatspec._
import matchers._

class InstructionsTest extends AnyFlatSpec with should.Matchers {

  "HLT" should "set the Halt flag to true" in {
    val ram = Memory(new Array[Byte](Memory.MAX_MEMORY)).writeByte(0x0000, 0x00)
    val cpu = new CPU(ram)
    val state = cpu.step
    state.flags.halt should be (true)
  }

  "HLT2" should "set the Halt flag to true" in {
    val ram = Memory(new Array[Byte](Memory.MAX_MEMORY)).writeByte(0x0000, 0xFF.toByte)
    val cpu = new CPU(ram)
    val state = cpu.step
    state.flags.halt should be (true)
  }

  "LMI" should "load the memory address in HL with data B...B" in {
    val ram = Memory(new Array[Byte](Memory.MAX_MEMORY)).writeByte(0x0000, 0x3E).writeByte(0x0001, 0x42)
    val cpu = new CPU(ram)
    cpu.state = cpu.state
      .withRegister(Registers.ID.H, 0x00).withRegister(Registers.ID.L, 0x01)
    val state = cpu.step
    state.ram.readByte(0x0001) should be (0x42)
  }

  "LrI" should "load the index register with data B...B" in {
    val ram = Memory(new Array[Byte](Memory.MAX_MEMORY)).writeByte(0x0000, 0x06).writeByte(0x0001, 0x42)
    val cpu = new CPU(ram)
    val state = cpu.step
    state.getRegister(Registers.ID.A) should be (0x42)
  }

  "LMr" should "load the memory address in HL with the contents of the index register" in {
    val ram = Memory(new Array[Byte](Memory.MAX_MEMORY)).writeByte(0x0000, 0xF8.toByte)
    val cpu = new CPU(ram)
    cpu.state = cpu.state.withRegister(Registers.ID.A, 0x42)
      .withRegister(Registers.ID.H, 0x00).withRegister(Registers.ID.L, 0x01)
    val state = cpu.step
    state.ram.readByte(0x0001) should be (0x42)
  }

  "LrM" should "load the index register with the contents of the memory address in HL" in {
    val ram = Memory(new Array[Byte](Memory.MAX_MEMORY)).writeByte(0x0000, 0xC7.toByte).writeByte(0x0001, 0x42)
    val cpu = new CPU(ram)
    cpu.state = cpu.state
      .withRegister(Registers.ID.H, 0x00).withRegister(Registers.ID.L, 0x01)
    val state = cpu.step
    state.getRegister(Registers.ID.A) should be (0x42)
  }

  "Lr1r2" should "load the index register r1 with the contents of index register r2" in {
    val ram = Memory(new Array[Byte](Memory.MAX_MEMORY)).writeByte(0x0000, 0xC8.toByte)
    val cpu = new CPU(ram)
    cpu.state = cpu.state.withRegister(Registers.ID.A, 0x42)
    val state = cpu.step
    state.getRegister(Registers.ID.B) should be (0x42)
  }
}