package net.mcribbs.s8008

import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*
import spire.math.UByte

class RegistersTest extends AnyFlatSpec with should.Matchers {

  "Registers" should "store a value" in {
    var r = Registers()
    r = r.withRegister(Register.A, UByte(0x01))
    r.getRegister(Register.A) should be (UByte(0x01))
  }

  it should "return the contents of H and L as HL" in {
    var r = Registers()
    r = r.withRegister(Register.H, UByte(0x11)).withRegister(Register.L, UByte(0x05))
    r.HL should be (0x1105)
  }
}