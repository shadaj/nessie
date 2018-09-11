package me.shadaj.nessie.instructions

import me.shadaj.nessie.{Instruction, NoArgs}
import me.shadaj.nessie.Instruction.setZeroNeg
import shapeless._

object TransferInstructions {
  val transferInstructions = Seq(
    Instruction[NoArgs :: HNil, NoArgs]("TAX", 0xAA) { (_, cpu) =>
      cpu.xRegister = cpu.accumulator
      setZeroNeg(cpu.xRegister, cpu)

      2
    },

    Instruction[NoArgs :: HNil, NoArgs]("TAY", 0xA8) { (_, cpu) =>
      cpu.yRegister = cpu.accumulator
      setZeroNeg(cpu.yRegister, cpu)

      2
    },

    Instruction[NoArgs :: HNil, NoArgs]("TSX", 0xBA) { (_, cpu) =>
      cpu.xRegister = cpu.stackPointer

      cpu.zeroFlag = cpu.xRegister == 0
      cpu.negativeFlag = cpu.xRegister < 0

      2
    },

    Instruction[NoArgs :: HNil, NoArgs]("TXA", 0x8A) { (_, cpu) =>
      cpu.accumulator = cpu.xRegister
      setZeroNeg(cpu.accumulator, cpu)

      2
    },

    Instruction[NoArgs :: HNil, NoArgs]("TXS", 0x9A) { (_, cpu) =>
      cpu.stackPointer = cpu.xRegister
      2
    },

    Instruction[NoArgs :: HNil, NoArgs]("TYA", 0x98) { (_, cpu) =>
      cpu.accumulator = cpu.yRegister
      setZeroNeg(cpu.accumulator, cpu)

      2
    }
  )
}
