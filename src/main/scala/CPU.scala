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
      // CPU control group
      case (0,0, 0,0,0, 0,0,_) => i.HLT                         // HLT
      case (1,1, 1,1,1, 1,1,1) => i.HLT                         // HLT

      // Input and output group
      case (0,1, 0,0,_, _,_,1) => ???                           // INP
      case (0,1, _,_,_, _,_,1) => ???                           // OUT

      // Jump group
      case (0,1, _,_,_, 1,0,0) => i.JMP(true)                   // JMP
      case (0,1, 0,0,0, 0,0,0) => i.JMP(!state.flags.carry)     // JFC
      case (0,1, 0,0,1, 0,0,0) => i.JMP(!state.flags.zero)      // JFZ
      case (0,1, 0,1,0, 0,0,0) => i.JMP(!state.flags.sign)      // JFS
      case (0,1, 0,1,1, 0,0,0) => i.JMP(!state.flags.parity)    // JFP
      case (0,1, 1,0,0, 0,0,0) => i.JMP(state.flags.carry)      // JC
      case (0,1, 1,0,1, 0,0,0) => i.JMP(state.flags.zero)       // JZ
      case (0,1, 1,1,0, 0,0,0) => i.JMP(state.flags.sign)       // JS
      case (0,1, 1,1,1, 0,0,0) => i.JMP(state.flags.parity)     // JP

      // Call and return group
      case (0,1, _,_,_, 1,1,0) => i.CAL(true)                   // CAL
      case (0,1, 0,0,0, 0,1,0) => i.CAL(!state.flags.carry)     // CFC
      case (0,1, 0,0,1, 0,1,0) => i.CAL(!state.flags.zero)      // CFZ
      case (0,1, 0,1,0, 0,1,0) => i.CAL(!state.flags.sign)      // CFS
      case (0,1, 0,1,1, 0,1,0) => i.CAL(!state.flags.parity)    // CFP
      case (0,1, 1,0,0, 0,1,0) => i.CAL(state.flags.carry)      // CC
      case (0,1, 1,0,1, 0,1,0) => i.CAL(state.flags.zero)       // CZ
      case (0,1, 1,1,0, 0,1,0) => i.CAL(state.flags.sign)       // CS
      case (0,1, 1,1,1, 0,1,0) => i.CAL(state.flags.parity)     // CP

      case (0,0, _,_,_, 1,1,1) => i.RET(true)                   // RET
      case (0,0, 0,0,0, 0,1,1) => i.RET(!state.flags.carry)     // RFC
      case (0,0, 0,0,1, 0,1,1) => i.RET(!state.flags.zero)      // RFZ
      case (0,0, 0,1,0, 0,1,1) => i.RET(!state.flags.sign)      // RFS
      case (0,0, 0,1,1, 0,1,1) => i.RET(!state.flags.parity)    // RFP
      case (0,0, 1,0,0, 0,1,1) => i.RET(state.flags.carry)      // RC
      case (0,0, 1,0,1, 0,1,1) => i.RET(state.flags.zero)       // RZ
      case (0,0, 1,1,0, 0,1,1) => i.RET(state.flags.sign)       // RS
      case (0,0, 1,1,1, 0,1,1) => i.RET(state.flags.parity)     // RP

      case (0,0, _,_,_, 1,0,1) => i.RST(ddd)                    // RST

      // Load group
      case (1,1, _,_,_, 1,1,1) => i.LrM(ddd)                    // LdM
      case (1,1, 1,1,1, _,_,_) => i.LMr(sss)                    // LMs
      case (1,1, _,_,_, _,_,_) => i.Lrr(ddd, sss)               // Lds

      case (0,0, 1,1,1, 1,1,0) => i.LMI                         // LMI
      case (0,0, _,_,_, 1,1,0) => i.LrI(ddd)                    // LdI

      // Arithmetic group
      case (1,0, 0,0,0, 1,1,1) => ???             // ADM
      case (0,0, 0,0,0, 1,0,0) => ???             // ADI
      case (1,0, 0,0,0, _,_,_) => ???             // ADs

      case (1,0, 0,0,1, 1,1,1) => ???             // ACM
      case (0,0, 0,0,1, 1,0,0) => ???             // ACI
      case (1,0, 0,0,1, _,_,_) => ???             // ACs

      case (1,0, 0,1,0, 1,1,1) => ???             // SUM
      case (0,0, 0,1,0, 1,0,0) => ???             // SUI
      case (1,0, 0,1,0, _,_,_) => ???             // SUs

      case (1,0, 0,1,1, 1,1,1) => ???             // SBM
      case (0,0, 0,1,1, 1,0,0) => ???             // SBI
      case (1,0, 0,1,1, _,_,_) => ???             // SBs

      case (1,0, 1,0,0, 1,1,1) => ???             // NDM
      case (0,0, 1,0,0, 1,0,0) => ???             // NDI
      case (1,0, 1,0,0, _,_,_) => ???             // NDs

      case (1,0, 1,0,1, 1,1,1) => ???             // XRM
      case (0,0, 1,0,1, 1,0,0) => ???             // XRI
      case (1,0, 1,0,1, _,_,_) => ???             // XRs

      case (1,0, 1,1,0, 1,1,1) => ???             // ORM
      case (0,0, 1,1,0, 1,0,0) => ???             // ORI
      case (1,0, 1,1,0, _,_,_) => ???             // ORs

      case (1,0, 1,1,1, 1,1,1) => ???             // CPM
      case (0,0, 1,1,1, 1,0,0) => ???             // CPI
      case (1,0, 1,1,1, _,_,_) => ???             // CPs

      case (0,0, _,_,_, 0,0,0) => i.INr(ddd)      // INd
      case (0,0, _,_,_, 0,0,1) => i.DCr(ddd)      // DCd

      // Rotate group
      case (0,0, 0,0,0, 0,1,0) => ???             // RLC
      case (0,0, 0,0,1, 0,1,0) => ???             // RRC
      case (0,0, 0,1,0, 0,1,0) => ???             // RAL
      case (0,0, 0,1,1, 0,1,0) => ???             // RAR
    }
    t2
  }