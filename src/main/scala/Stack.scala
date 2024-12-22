package net.mcribbs.s8008

// https://retrocomputing.stackexchange.com/questions/15787/intel-8008-stack-behavior
class Stack (sp: Int = 0, s: Array[Short] = Array.ofDim[Short](Stack.MAX_SIZE)):

  def push(address: Short): Stack = 
    val newStackPointer = (this.sp + 1) % Stack.MAX_SIZE
    // Again, pretending to be immutable with the mutable Array... Ok????
    val newStackBuffer = this.s.clone()
    newStackBuffer(newStackPointer) = address
    new Stack(newStackPointer, newStackBuffer)

  def decSP: Stack = 
    val newStackPointer = (sp + Stack.MAX_SIZE - 1) % Stack.MAX_SIZE
    new Stack(newStackPointer, this.s)

  def PC: Short = s(sp)
  
  def withPC(newPC: Short): Stack =
    val newStackBuffer = this.s.clone()
    newStackBuffer(sp) = newPC 
    new Stack(sp, newStackBuffer)
  
  def incPC: Stack =
    val newStackBuffer = this.s.clone()
    newStackBuffer(sp) = (s(sp) + 1).toShort
    new Stack(sp, newStackBuffer)

  override def toString:String = 
    f"PC:$PC%#06x " +
    s"Stack(" + s.map(n => f"$n%#06x").mkString(", ") + ") " +
    f"sp:$sp%#04x "

object Stack:
  val MAX_SIZE = 8