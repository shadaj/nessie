package me.shadaj.nessie.instructions

import me.shadaj.nessie.{Instruction, Relative}
import shapeless._

object BranchInstructions {
  val branchInstructions = Seq(
    Instruction[Relative :: HNil](0x30, "BMI") { case (Relative(relativeAddr) :: HNil, cpu) =>
      if (cpu.negativeFlag) {
        cpu.programCounter += relativeAddr
        3 // TODO: different cycle count for new page
      } else {
        2
      }
    },

    Instruction[Relative :: HNil](0x90, "BCC") { case (Relative(relativeAddr) :: HNil, cpu) =>
      if (!cpu.carryFlag) {
        cpu.programCounter += relativeAddr
        3 // TODO: different cycle count for new page
      } else {
        2
      }
    },

    Instruction[Relative :: HNil](0xB0, "BCS") { case (Relative(relativeAddr) :: HNil, cpu) =>
      if (cpu.carryFlag) {
        cpu.programCounter += relativeAddr
        3 // TODO: different cycle count for new page
      } else {
        2
      }
    },

    Instruction[Relative :: HNil](0xF0, "BEQ") { case (Relative(relativeAddr) :: HNil, cpu) =>
      if (cpu.zeroFlag) {
        cpu.programCounter += relativeAddr
        3 // TODO: different cycle count for new page
      } else {
        2
      }
    },

    Instruction[Relative :: HNil](0xD0, "BNE") { case (Relative(relativeAddr) :: HNil, cpu) =>
      if (!cpu.zeroFlag) {
        cpu.programCounter += relativeAddr
        3 // TODO: different cycle count for new page
      } else {
        2
      }
    },

    Instruction[Relative :: HNil](0x10, "BPL") { case (Relative(relativeAddr) :: HNil, cpu) =>
      if (!cpu.negativeFlag) {
        cpu.programCounter += relativeAddr
        3 // TODO: different cycle count for new page
      } else {
        2
      }
    },

    Instruction[Relative :: HNil](0x50, "BVC") { case (Relative(relativeAddr) :: HNil, cpu) =>
      if (!cpu.overflowFlag) {
        cpu.programCounter += relativeAddr
        3 // TODO: different cycle count for new page
      } else {
        2
      }
    },

    Instruction[Relative :: HNil](0x70, "BVS") { case (Relative(relativeAddr) :: HNil, cpu) =>
      if (cpu.overflowFlag) {
        cpu.programCounter += relativeAddr
        3 // TODO: different cycle count for new page
      } else {
        2
      }
    }
  )
}
