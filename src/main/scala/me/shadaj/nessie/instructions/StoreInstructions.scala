package me.shadaj.nessie.instructions

import me.shadaj.nessie._
import shapeless._

object StoreInstructions {
  val storeInstructions = Seq(
    Instruction[ZeroPage :: ZeroPageX :: Absolute :: AbsoluteX :: AbsoluteY :: IndirectX :: IndirectIndexed :: HNil, Address]("STA",
      0x85, 0x95, 0x8D, 0x9D, 0x99, 0x81, 0x91) { (ad, cpu) =>
      cpu.memory.write(ad.address, cpu.accumulator)

      ad match {
        case _: ZeroPage => 3
        case _: ZeroPageX => 4
        case _: Absolute => 4
        case _: AbsoluteX => 5
        case _: AbsoluteY => 5
        case _: IndirectX => 6
        case _: IndirectIndexed => 6
      }
    },

    Instruction[ZeroPage :: ZeroPageY :: Absolute :: HNil, Address]("STX", 0x86, 0x96, 0x8E) { (addr, cpu) =>
      cpu.memory.write(addr.address, cpu.xRegister)

      addr match {
        case _: ZeroPage => 3
        case _: ZeroPageY => 4
        case _: Absolute => 4
      }
    },

    Instruction[ZeroPage :: ZeroPageX :: Absolute :: HNil, Address]("STY", 0x84, 0x94, 0x8C) { (addr, cpu) =>
      cpu.memory.write(addr.address, cpu.yRegister)

      addr match {
        case _: ZeroPage => 3
        case _: ZeroPageX => 4
        case _: Absolute => 4
      }
    }
  )
}
