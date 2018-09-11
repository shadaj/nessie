package me.shadaj.nessie.instructions

import me.shadaj.nessie.{Instruction, Relative}
import shapeless._

object BranchInstructions {
  val branchInstructions = Seq(
    Instruction[Relative :: HNil, Relative]("BMI", 0x30) { (relative, cpu) =>
      if (cpu.negativeFlag) {
        val orig = cpu.programCounter
        cpu.programCounter += relative.relativeAddress
        3 + Instruction.pageCrossExtra(orig, cpu.programCounter)
      } else {
        2
      }
    },

    Instruction[Relative :: HNil, Relative]("BCC", 0x90) { (relative, cpu) =>
      if (!cpu.carryFlag) {
        val orig = cpu.programCounter
        cpu.programCounter += relative.relativeAddress
        3 + Instruction.pageCrossExtra(orig, cpu.programCounter)
      } else {
        2
      }
    },

    Instruction[Relative :: HNil, Relative]("BCS", 0xB0) { (relative, cpu) =>
      if (cpu.carryFlag) {
        val orig = cpu.programCounter
        cpu.programCounter += relative.relativeAddress
        3 + Instruction.pageCrossExtra(orig, cpu.programCounter)
      } else {
        2
      }
    },

    Instruction[Relative :: HNil, Relative]("BEQ", 0xF0) { (relative, cpu) =>
      if (cpu.zeroFlag) {
        val orig = cpu.programCounter
        cpu.programCounter += relative.relativeAddress
        3 + Instruction.pageCrossExtra(orig, cpu.programCounter)
      } else {
        2
      }
    },

    Instruction[Relative :: HNil, Relative]("BNE", 0xD0) { (relative, cpu) =>
      if (!cpu.zeroFlag) {
        val orig = cpu.programCounter
        cpu.programCounter += relative.relativeAddress
        3 + (Instruction.pageCrossExtra(orig, cpu.programCounter) * 1)
      } else {
        2
      }
    },

    Instruction[Relative :: HNil, Relative]("BPL", 0x10) { (relative, cpu) =>
      if (!cpu.negativeFlag) {
        val orig = cpu.programCounter
        cpu.programCounter += relative.relativeAddress
        3 + Instruction.pageCrossExtra(orig, cpu.programCounter)
      } else {
        2
      }
    },

    Instruction[Relative :: HNil, Relative]("BVC", 0x50) { (relative, cpu) =>
      if (!cpu.overflowFlag) {
        val orig = cpu.programCounter
        cpu.programCounter += relative.relativeAddress
        3 + Instruction.pageCrossExtra(orig, cpu.programCounter)
      } else {
        2
      }
    },

    Instruction[Relative :: HNil, Relative]("BVS", 0x70) { (relative, cpu) =>
      if (cpu.overflowFlag) {
        val orig = cpu.programCounter
        cpu.programCounter += relative.relativeAddress
        3 + Instruction.pageCrossExtra(orig, cpu.programCounter)
      } else {
        2
      }
    }
  )
}
