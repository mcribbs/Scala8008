package net.mcribbs.s8008

import org.scalatest._
import flatspec._
import matchers._

class MemoryTest extends AnyFlatSpec with should.Matchers {

  "Memory" should "store a value at an address and retrieve it" in {
    var ram = Memory(new Array[Byte](Memory.MAX_MEMORY))
    ram = ram.writeByte(0x0002, 0x42)
    ram.readByte(0x0002) should be (0x42)
  }
}