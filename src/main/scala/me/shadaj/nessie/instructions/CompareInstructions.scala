package me.shadaj.nessie.instructions

import java.lang.Byte.toUnsignedInt

import me.shadaj.nessie.Instruction.{AllAddressTypes, pageCrossExtra, setZeroNeg}
import me.shadaj.nessie.{Absolute, AbsoluteX, AbsoluteY, Immediate, IndirectIndexed, IndirectX, Instruction, Readable, ZeroPage, ZeroPageX}
import shapeless._

object CompareInstructions {
  val compareInstructions = Seq(
    Instruction[AllAddressTypes, Readable]("CMP",
      0xC9, 0xC5, 0xD5, 0xCD, 0xDD, 0xD9, 0xC1, 0xD1
    ) { (addr, cpu) =>
      val result = toUnsignedInt(cpu.accumulator) - toUnsignedInt(addr.getValue(cpu, cpu.memory))
      cpu.carryFlag = (result & 0x100) == 0
      setZeroNeg(result.toByte, cpu)

      addr match {
        case _: Immediate => 2
        case _: ZeroPage => 3
        case _: ZeroPageX => 4
        case _: Absolute => 4
        case a: AbsoluteX => 4 + pageCrossExtra(a.absolute, a.address)
        case a: AbsoluteY => 4 + pageCrossExtra(a.absolute, a.address)
        case _: IndirectX => 6
        case a: IndirectIndexed => 5 + pageCrossExtra(a.indirect, a.address)
      }
    },
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
