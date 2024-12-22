/*
  Option 1: Use a map
 */
enum Registers1:
  case A,B,C,D,E,H,L
  
case class CPUState1
(
  registers: Map[Registers1, Byte] =  Map[Registers1, Byte](
    (Registers1.A, 0x00), (Registers1.B, 0x00),
    (Registers1.C, 0x00), (Registers1.D, 0x00),
    (Registers1.E, 0x00), (Registers1.H, 0x00),
    (Registers1.L, 0x00))
)

// CPU start
val initialState1 = CPUState1()

// Things happen

// Decode register from instruction bits
val bits1 = 4 // binary 100 -> Register D
val r1 = bits1 match {
  case 4 => Registers1.D
}

// Other things happen

// Apply new value to state
val nextState1 = initialState1.copy(registers = 
  initialState1.registers + (r1 -> 0x05))

/* 
  Option 2: Case class
 */
case class Registers2(A: Byte = 0, B: Byte = 0, 
                      C: Byte = 0, D: Byte = 0, 
                      E: Byte = 0, H: Byte = 0, 
                      L: Byte = 0) 

case class CPUState2 ( registers: Registers2 = Registers2()) {
  def withRegister(regId: Byte, value: Byte): CPUState2 = {
    copy(registers = regId match {
      case 4 => this.registers.copy(D = value)
    })
  }
}

// CPU start
val initialState2 = CPUState2()

// Things happen

// Decode register from instruction bits
val bits2 = 4 // binary 100 -> Register D

// Other things happen

// Apply new value to state, but how?
val nextState2 = initialState2.withRegister(bits2.byteValue, 0x05)


//------------------------------------
class StackPointerTest(sp: Int):
  def increment: StackPointerTest = new StackPointerTest((sp + 1) % 8)
  def decrement: StackPointerTest = new StackPointerTest((sp + 8 - 1) % 8)

  override def toString: String = "sp = " + sp.toString
  
val sp1 = new StackPointerTest(5);
val sp2 = sp1.increment; // 6
val sp3 = sp2.increment; // 7 
val sp4 = sp3.increment; // 0 
val sp5 = sp4.increment; // 1 
val sp6 = sp5.increment; // 2
val sp7 = sp6.decrement; // 1 
val sp8 = sp7.decrement; // 0 
val sp9 = sp8.decrement; // 7 
val sp10 = sp9.decrement; // 6 
val sp11 = sp10.decrement; // 5
