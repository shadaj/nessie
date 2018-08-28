package me.shadaj.nessie.instructions

import me.shadaj.nessie.{Instruction, Relative}
import shapeless._

object BranchInstructions {
  val branchInstructions = Seq(
    Instruction[Relative :: HNil, Relative]("BMI", 0x30) { (relative, cpu) =>
      if (cpu.negativeFlag) {
        cpu.programCounter += relative.relativeAddress
        3 // TODO: different cycle count for new page
      } else {
        2
      }
    },

    Instruction[Relative :: HNil, Relative]("BCC", 0x90) { (relative, cpu) =>
      if (!cpu.carryFlag) {
        cpu.programCounter += relative.relativeAddress
        3 // TODO: different cycle count for new page
      } else {
        2
      }
    },

    Instruction[Relative :: HNil, Relative]("BCS", 0xB0) { (relative, cpu) =>
      if (cpu.carryFlag) {
        cpu.programCounter += relative.relativeAddress
        3 // TODO: different cycle count for new page
      } else {
        2
      }
    },

    Instruction[Relative :: HNil, Relative]("BEQ", 0xF0) { (relative, cpu) =>
      if (cpu.zeroFlag) {
        cpu.programCounter += relative.relativeAddress
        3 // TODO: different cycle count for new page
      } else {
        2
      }
    },

    Instruction[Relative :: HNil, Relative]("BNE", 0xD0) { (relative, cpu) =>
      if (!cpu.zeroFlag) {
        cpu.programCounter += relative.relativeAddress
        3 // TODO: different cycle count for new page
      } else {
        2
      }
    },

    Instruction[Relative :: HNil, Relative]("BPL", 0x10) { (relative, cpu) =>
      if (!cpu.negativeFlag) {
        cpu.programCounter += relative.relativeAddress
        3 // TODO: different cycle count for new page
      } else {
        2
      }
    },

    Instruction[Relative :: HNil, Relative]("BVC", 0x50) { (relative, cpu) =>
      if (!cpu.overflowFlag) {
        cpu.programCounter += relative.relativeAddress
        3 // TODO: different cycle count for new page
      } else {
        2
      }
    },

    Instruction[Relative :: HNil, Relative]("BVS", 0x70) { (relative, cpu) =>
      if (cpu.overflowFlag) {
        cpu.programCounter += relative.relativeAddress
        3 // TODO: different cycle count for new page
      } else {
        2
      }
    }
  )
}
