package net.mcribbs.s8008

import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*
import spire.math.UByte

class StackTest extends AnyFlatSpec with should.Matchers {

  "A Stack" should "increment the PC" in {
    var s = Stack()
    s = s.incPC
    s.PC should be(0x0001)
  }

  it should "store a new value in PC" in {
    var s = Stack()
    s = s.withPC(((UByte(0x02).toShort << 8) +  UByte(0x04).toShort).toShort)
    s.PC should be(0x0204)
  }

  it should "store a value" in {
    var s = Stack()
    s = s.push(0x00FF)
    s.PC should be(0x00FF)
  }

  it should "return the previous value when SP decremented" in {
    var s = Stack()
    s = s.incPC.push(0x00FF).decSP
    s.PC should be(0x0001)
  }
}