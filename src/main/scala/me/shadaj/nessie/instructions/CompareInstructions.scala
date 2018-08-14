package me.shadaj.nessie.instructions

import java.lang.Byte.toUnsignedInt

import me.shadaj.nessie.Instruction.processNegativeFlag
import me.shadaj.nessie.{Absolute, Immediate, Instruction, ZeroPage}

object CompareInstructions {
  val compareInstructions =
    Instruction.generateNonIndirectNoRegisterTypes("CPX")(
      0xE0, 0xE4, 0xEC
    ) { (value, addr, cpu) =>
      cpu.carryFlag = toUnsignedInt(cpu.xRegister) >= toUnsignedInt(value)
      cpu.zeroFlag = toUnsignedInt(cpu.xRegister) == toUnsignedInt(value)
      cpu.negativeFlag = processNegativeFlag(cpu.xRegister, value <= 0) < value

      addr match {
        case _: Immediate => 2
        case _: ZeroPage => 3
        case _: Absolute => 4
      }
    } ++
    Instruction.generateNonIndirectNoRegisterTypes("CPY")(
      0xC0, 0xC4, 0xCC
    ) { (value, addr, cpu) =>
      cpu.carryFlag = toUnsignedInt(cpu.yRegister) >= toUnsignedInt(value)
      cpu.zeroFlag = toUnsignedInt(cpu.yRegister) == toUnsignedInt(value)
      cpu.negativeFlag = processNegativeFlag(cpu.yRegister, value <= 0) < value

      addr match {
        case _: Immediate => 2
        case _: ZeroPage => 3
        case _: Absolute => 4
      }
    }
}
