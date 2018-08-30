package me.shadaj.nessie.instructions

import java.lang.Byte.toUnsignedInt

import me.shadaj.nessie.Instruction.setZeroNeg
import me.shadaj.nessie.{Absolute, Immediate, Instruction, ZeroPage, Readable}
import shapeless._

object CompareInstructions {
  val compareInstructions = Seq(
    Instruction[Immediate :: ZeroPage :: Absolute :: HNil, Readable]("CPX", 0xE0, 0xE4, 0xEC) { (addr, cpu) =>
      val result = toUnsignedInt(cpu.xRegister) - toUnsignedInt(addr.getValue(cpu, cpu.memory))
      cpu.carryFlag = (result & 0x100) == 0
      setZeroNeg(result.toByte, cpu)

      addr match {
        case _: Immediate => 2
        case _: ZeroPage => 3
        case _: Absolute => 4
      }
    },
    Instruction[Immediate :: ZeroPage :: Absolute :: HNil, Readable]("CPY", 0xC0, 0xC4, 0xCC) { (addr, cpu) =>
      val result = toUnsignedInt(cpu.yRegister) - toUnsignedInt(addr.getValue(cpu, cpu.memory))
      cpu.carryFlag = (result & 0x100) == 0
      setZeroNeg(result.toByte, cpu)

      addr match {
        case _: Immediate => 2
        case _: ZeroPage => 3
        case _: Absolute => 4
      }
    }
  )
}
