package me.shadaj.nessie.instructions

import me.shadaj.nessie.Instruction.setZeroNeg
import me.shadaj.nessie._
import shapeless._

object LoadInstructions {
  val loadInstructions = Instruction.generateAllAddressTypes("LDA")(
    0xA9, 0xA5, 0xB5, 0xAd, 0xBd, 0xB9, 0xA1, 0xB1
  ) { (value, addr, cpu) => {
    cpu.accumulator = value
    setZeroNeg(cpu.accumulator, cpu)

    addr match {
      case _: Immediate => 2
      case _: ZeroPage => 3
      case _: ZeroPageX => 4
      case _: Absolute => 4
      case _: AbsoluteX => 4 // TODO: page cross
      case _: AbsoluteY => 4
      case _: IndirectX => 6
      case _: IndirectIndexed => 5
    }
  }} ++ Instruction.generateNonIndirectXAddressTypes("LDX")(
    0xA2, 0xA6, 0xB6, 0xAE, 0xBE
  ) { (value, addr, cpu) =>
    cpu.xRegister = value
    setZeroNeg(cpu.xRegister, cpu)

    addr match {
      case _: Immediate => 2
      case _: ZeroPage => 3
      case _: ZeroPageX => 4
      case _: Absolute => 4
      case _: AbsoluteX => 4 // TODO: page cross
    }
  } ++ Instruction.generateNonIndirectXAddressTypes("LDY")(
    0xA0, 0xA4, 0xB4, 0xAC, 0xBC
  ) { (value, addr, cpu) =>
    cpu.yRegister = value
    setZeroNeg(cpu.yRegister, cpu)

    addr match {
      case _: Immediate => 2
      case _: ZeroPage => 3
      case _: ZeroPageX => 4
      case _: Absolute => 4
      case _: AbsoluteX => 4 // TODO: page cross
    }
  }
}
