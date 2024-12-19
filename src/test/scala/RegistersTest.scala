package net.mcribbs.s8008

import collection.mutable.Stack
import org.scalatest._
import flatspec._
import matchers._

class RegistersTest extends AnyFlatSpec with should.Matchers {

  "Registers" should "store a value" in {
    var r = Registers()
    r = r.withRegister(Registers.ID.A, 0x01)
    r.getRegister(Registers.ID.A) should be (0x01)
  }

  it should "return the contents of H and L as HL" in {
    var r = Registers()
    r = r.withRegister(Registers.ID.H, 0x11).withRegister(Registers.ID.L, 0x05)
    r.HL should be (0x1105)
  }
}