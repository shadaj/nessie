package me.shadaj.nessie.instructions

import me.shadaj.nessie.{Arg, Instruction}
import me.shadaj.nessie.Instruction.setZeroNeg
import shapeless.HNil
import shapeless.ops.hlist.{ToList, ToTraversable}

object TransferInstructions {
  implicit val nilToList: ToList[HNil, Arg] = ToTraversable.hnilToTraversable[HNil, List, Arg]

  val transferInstructions = Seq(
    Instruction[HNil](0xAA, "TAX") { (_, cpu) =>
      cpu.xRegister = cpu.accumulator
      setZeroNeg(cpu.xRegister, cpu)

      2
    },

    Instruction[HNil](0xA8, "TAY") { (_, cpu) =>
      cpu.yRegister = cpu.accumulator
      setZeroNeg(cpu.yRegister, cpu)

      2
    },

    Instruction[HNil](0xBA, "TSX") { (_, cpu) =>
      cpu.xRegister = cpu.stackPointer

      cpu.zeroFlag = cpu.xRegister == 0
      cpu.negativeFlag = cpu.xRegister < 0

      2
    },

    Instruction[HNil](0x8A, "TXA") { (_, cpu) =>
      cpu.accumulator = cpu.xRegister
      setZeroNeg(cpu.accumulator, cpu)

      2
    },

    Instruction[HNil](0x9A, "TXS") { (_, cpu) =>
      cpu.stackPointer = cpu.xRegister
      2
    },

    Instruction[HNil](0x98, "TYA") { (_, cpu) =>
      cpu.accumulator = cpu.yRegister
      setZeroNeg(cpu.accumulator, cpu)

      2
    }
  )
}
