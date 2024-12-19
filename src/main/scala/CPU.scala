package net.mcribbs.s8008

class CPU(ram: Memory):
  var state: CPUState = CPUState(ram = ram)

  def step: CPUState = {
    /*
      The Intel 8008 microprocessor's processor cycle, also known as the M cycle, consists of the following T-states:
        T1: Outputs the low 8-bits of the address
        T2: Outputs the high 6-bits of the address, plus two control bits
        T3: Reads an instruction or data, or outputs data to be written
        T4: Instruction execution, which is optional depending on the instruction
        T5: Instruction execution, which is optional depending on the instruction

      We're going to use a similar concept to manage state
     */

    val opcode: Byte = state.ram.readByte(state.PC)
    val t1 = state.incrementPC

    // Chunk opcode into useful parts
    val d7 = (opcode & 0x80) >> 7
    val d6 = (opcode & 0x40) >> 6
    val d5 = (opcode & 0x20) >> 5
    val d4 = (opcode & 0x10) >> 4
    val d3 = (opcode & 0x08) >> 3
    val d2 = (opcode & 0x04) >> 2
    val d1 = (opcode & 0x02) >> 1
    val d0 = (opcode & 0x01) >> 0 
    val ddd: Byte = ((opcode & 0x38) >> 3).toByte
    val sss: Byte = (opcode & 0x07).toByte

    val i: Instructions = new Instructions(t1)
    val t2: CPUState = (d7,d6,d5,d4,d3,d2,d1,d0) match {
      case (0,0, 0,0,0, 0,0,0) => i.HLT
      case (0,0, 1,1,1, 1,1,0) => i.LMI
      case (0,0, _,_,_, 1,1,0) => i.LrI(ddd)
      case (0,0, _,_,_, _,_,_) => ???
      case (0,1, _,_,_, _,_,_) => ???
      case (1,0, _,_,_, _,_,_) => ???
      case (1,1, 1,1,1, 1,1,1) => i.HLT
      case (1,1, 1,1,1, _,_,_) => i.LMr(sss)                                                   // LrM
      case (1,1, _,_,_, 1,1,1) => i.LrM(ddd)                                                   // LMr
      case (1,1, _,_,_, _,_,_) => i.Lrr(ddd, sss)
    }
    t2
  }