package net.mcribbs.s8008

import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*
import spire.math.UByte

class MemoryTest extends AnyFlatSpec with should.Matchers {

  "Memory" should "store a value at an address and retrieve it" in {
    var ram = Memory(new Array[UByte](Memory.MAX_MEMORY))
    ram = ram.writeByte(0x0002, UByte(0x42))
    ram.readByte(0x0002) should be (UByte(0x42))
  }
}