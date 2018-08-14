package me.shadaj.nessie.instructions

import me.shadaj.nessie._
import shapeless._

object StoreInstructions {
  val storeInstructions = Seq(
    Instruction[ZeroPage :: HNil](0x85, "STA") { case (ad :: HNil, cpu) =>
      cpu.memory.write(ad.address, cpu.accumulator)
      3
    },
    Instruction[ZeroPageX :: HNil](0x95, "STA") { case (ad :: HNil, cpu) =>
      cpu.memory.write(ad.address, cpu.accumulator)
      4
    },
    Instruction[Absolute :: HNil](0x8D, "STA") { case (ad :: HNil, cpu) =>
      cpu.memory.write(ad.twoBytes, cpu.accumulator)
      4
    },
    Instruction[AbsoluteX :: HNil](0x9D, "STA") { case (ad :: HNil, cpu) =>
      cpu.memory.write(ad.address, cpu.accumulator)
      5
    },
    Instruction[AbsoluteY :: HNil](0x99, "STA") { case (ad :: HNil, cpu) =>
      cpu.memory.write(ad.address, cpu.accumulator)
      5
    },
    Instruction[IndirectX:: HNil](0x81, "STA") { case (ind :: HNil, cpu) =>
      cpu.memory.write(ind.address, cpu.accumulator)
      6
    },
    Instruction[IndirectIndexed :: HNil](0x91, "STA") { case (ind :: HNil, cpu) =>
      cpu.memory.write(ind.address, cpu.accumulator)
      6
    },

    Instruction[ZeroPage :: HNil](0x86, "STX") { case (ZeroPage(address) :: HNil, cpu) =>
      cpu.memory.write(address, cpu.xRegister)
      3
    },
    Instruction[ZeroPageY :: HNil](0x96, "STX") { case (addr :: HNil, cpu) =>
      cpu.memory.write(addr.address, cpu.xRegister)
      3
    },
    Instruction[Absolute :: HNil](0x8E, "STX") { case (Absolute(address) :: HNil, cpu) =>
      cpu.memory.write(address, cpu.xRegister)
      4
    },

    Instruction[ZeroPage :: HNil](0x84, "STY") { case (addr :: HNil, cpu) =>
      cpu.memory.write(addr.address, cpu.yRegister)
      3
    },
    Instruction[ZeroPageX :: HNil](0x94, "STY") { case (addr :: HNil, cpu) =>
      cpu.memory.write(addr.address, cpu.yRegister)
      4
    },
    Instruction[Absolute :: HNil](0x8C, "STY") { case (addr :: HNil, cpu) =>
      cpu.memory.write(addr.address, cpu.yRegister)
      4
    }
  )
}
