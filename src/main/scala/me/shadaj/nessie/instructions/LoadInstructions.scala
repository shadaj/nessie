package me.shadaj.nessie.instructions

import me.shadaj.nessie.Instruction
import me.shadaj.nessie.Instruction.{AllAddressTypes, setZeroNeg}
import me.shadaj.nessie._
import shapeless._

object LoadInstructions {
  val loadInstructions = Seq(
    Instruction[AllAddressTypes, Readable]("LDA",
      0xA9, 0xA5, 0xB5, 0xAd, 0xBd, 0xB9, 0xA1, 0xB1
    ) { (addr, cpu) =>
      cpu.accumulator = addr.getValue(cpu, cpu.memory)
      setZeroNeg(cpu.accumulator, cpu)

      addr match {
        case _: Immediate => 2
        case _: ZeroPage => 3
        case _: ZeroPageX => 4
        case _: Absolute => 4
        case a: AbsoluteX => 4 + Instruction.pageCrossExtra(a.absolute, a.address)
        case a: AbsoluteY => 4 + Instruction.pageCrossExtra(a.absolute, a.address)
        case _: IndirectX => 6
        case a: IndirectIndexed => 5 + Instruction.pageCrossExtra(a.indirect, a.address)
      }
    },
    Instruction[Immediate :: ZeroPage :: ZeroPageY :: Absolute :: AbsoluteY :: HNil, Readable]("LDY",
      0xA2, 0xA6, 0xB6, 0xAE, 0xBE
    ) { (addr, cpu) =>
      cpu.xRegister = addr.getValue(cpu, cpu.memory)
      setZeroNeg(cpu.xRegister, cpu)

      addr match {
        case _: Immediate => 2
        case _: ZeroPage => 3
        case _: ZeroPageY => 4
        case _: Absolute => 4
        case a: AbsoluteY => 4 + Instruction.pageCrossExtra(a.absolute, a.address)
      }
    },
    Instruction[Immediate :: ZeroPage :: ZeroPageX :: Absolute :: AbsoluteX :: HNil, Readable]("LDY",
      0xA0, 0xA4, 0xB4, 0xAC, 0xBC
    ) { (addr, cpu) =>
      cpu.yRegister = addr.getValue(cpu, cpu.memory)
      setZeroNeg(cpu.yRegister, cpu)

      addr match {
        case _: Immediate => 2
        case _: ZeroPage => 3
        case _: ZeroPageX => 4
        case _: Absolute => 4
        case a: AbsoluteX => 4 + Instruction.pageCrossExtra(a.absolute, a.address)
      }
    }
  )
}
