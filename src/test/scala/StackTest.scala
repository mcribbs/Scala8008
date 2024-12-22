package net.mcribbs.s8008

import org.scalatest._
import flatspec._
import matchers._

class StackTest extends AnyFlatSpec with should.Matchers {

  "A Stack" should "increment the PC" in {
    var s = Stack()
    s = s.incPC
    s.PC should be(0x01)
  }

  it should "store a new value in PC" in {
    var s = Stack()
    s = s.withPC(((0x02 << 8) + 0x04).asInstanceOf[Short])
    s.PC should be(0x0204)
  }

  it should "store a value" in {
    var s = Stack()
    s = s.push(0xFF)
    s.PC should be(0xFF)
  }

  it should "return the previous value when SP decremented" in {
    var s = Stack()
    s = s.incPC.push(0xFF).decSP
    s.PC should be(0x01)
  }
}