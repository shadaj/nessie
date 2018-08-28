package me.shadaj.nessie.instructions

import java.lang.Byte.toUnsignedInt

import me.shadaj.nessie.Instruction.setZeroNeg
import me.shadaj.nessie.{Absolute, Immediate, Instruction, ZeroPage}

object CompareInstructions {
  val compareInstructions =
    Instruction.generateNonIndirectNoRegisterTypes("CPX")(
      0xE0, 0xE4, 0xEC
    ) { (value, addr, cpu) =>
      val result = toUnsignedInt(cpu.xRegister) - toUnsignedInt(value)
      cpu.carryFlag = (result & 0x100) == 0
      setZeroNeg(result.toByte, cpu)

      addr match {
        case _: Immediate => 2
        case _: ZeroPage => 3
        case _: Absolute => 4
      }
    } ++
    Instruction.generateNonIndirectNoRegisterTypes("CPY")(
      0xC0, 0xC4, 0xCC
    ) { (value, addr, cpu) =>
      val result = toUnsignedInt(cpu.yRegister) - toUnsignedInt(value)
      cpu.carryFlag = (result & 0x100) == 0
      setZeroNeg(result.toByte, cpu)

      addr match {
        case _: Immediate => 2
        case _: ZeroPage => 3
        case _: Absolute => 4
      }
    }
}
