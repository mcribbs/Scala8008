package net.mcribbs.s8008

// https://retrocomputing.stackexchange.com/questions/15787/intel-8008-stack-behavior
case class Stack (sp: Int = 0, s: Vector[Short] = Vector.fill[Short](Stack.MAX_SIZE)(0x0000)):

  def push(address: Short): Stack =
    val newStackPointer = (this.sp + 1) % Stack.MAX_SIZE
    Stack(newStackPointer, this.s.updated(newStackPointer, address))

  def decSP: Stack = 
    val newStackPointer = (sp + Stack.MAX_SIZE - 1) % Stack.MAX_SIZE
    Stack(newStackPointer, this.s)

  def PC: Short = s(sp)
  
  def withPC(newPC: Short): Stack =
    Stack(sp, this.s.updated(sp, newPC))
  
  def incPC: Stack =
    Stack(sp, this.s.updated(sp, (s(sp) + 1).toShort))

  override def toString:String = 
    f"PC:$PC%#06x " +
    s"Stack(" + s.map(n => f"$n%#06x").mkString(", ") + ") " +
    f"sp:$sp%#04x "

object Stack:
  private val MAX_SIZE = 8