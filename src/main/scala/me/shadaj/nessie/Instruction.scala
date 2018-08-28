package me.shadaj.nessie

import shapeless._
import shapeless.ops.hlist.{ToList, ToTraversable}
import java.lang.Byte.toUnsignedInt

import me.shadaj.nessie.instructions._

trait Arg

trait ArgParser[A <: Arg] {
  val size: Int
  def parse(getArg: Int => Byte, cpu: CPU): A
}

trait ArgsParser[Args <: HList] {
  val parsers: Seq[ArgParser[_]]
}

object ArgsParser {
  implicit val forNil: ArgsParser[HNil] = new ArgsParser[HNil] {
    override val parsers: Seq[ArgParser[_]] = Seq.empty
  }

  implicit def forCons[Head <: Arg, Tail <: HList](implicit argParser: ArgParser[Head], tailParser: ArgsParser[Tail]): ArgsParser[Head :: Tail] = new ArgsParser[Head :: Tail] {
    val parsers = argParser +: tailParser.parsers
  }
}

case class Instruction[Args <: HList, LubA](name: String, opcodes: Seq[Int]*)
                                           (execute: (LubA, CPU) => Int)
                                           (implicit parseArgs: ArgsParser[Args]) {
  if (opcodes.size != parseArgs.parsers.size) {
    throw new IllegalArgumentException("Opcodes and parsers must be the same size")
  }

  val opcodesToArgs = opcodes.zip(parseArgs.parsers).flatMap(t => t._1.map(o => o.toByte -> t._2.asInstanceOf[ArgParser[Nothing]]))
    .asInstanceOf[Seq[(Byte, ArgParser[Nothing])]].toMap

  def run(opcode: Byte, getArg: Int => Byte, log: Boolean = false)(cpu: CPU) = {
    val parsedArg = opcodesToArgs(opcode).parse(getArg, cpu).asInstanceOf[LubA]
    if (log) {
      println(s"$this $parsedArg")
    }
    execute(parsedArg, cpu)
  }

  override def toString: String = name
}

class SeparateApply
object SeparateApply {
  implicit val imp = new SeparateApply
}

object Instruction {
  def apply[Args <: HList, LubA](name: String, opcodes: Int*)
                                (execute: (LubA, CPU) => Int)
                                (implicit parseArgs: ArgsParser[Args], separate: SeparateApply): Instruction[Args, LubA] = {
    apply[Args, LubA](name, opcodes.map(Seq(_)): _*)(execute)(parseArgs)
  }

  def pageCrossExtra(original: Int, shifted: Int) = {
    if ((original & 0xFF00) != (shifted & 0xFF00)) 1 else 0
  }

  def setZeroNeg(value: Byte, cpu: CPU) = {
    cpu.zeroFlag = value == 0
    cpu.negativeFlag = value < 0
  }

  def pushToStack(value: Byte, cpu: CPU) = {
    cpu.memory.write(0x0100 | toUnsignedInt(cpu.stackPointer), value)
    cpu.stackPointer = (cpu.stackPointer - 1).toByte
  }

  def popFromStack(cpu: CPU): Byte = {
    cpu.stackPointer = (cpu.stackPointer + 1).toByte
    val ret = cpu.memory.read(0x0100 | toUnsignedInt(cpu.stackPointer))
    ret
  }

  implicit val nilToList: ToList[HNil, Arg] = ToTraversable.hnilToTraversable[HNil, List, Arg]

  def generateAllAddressTypes(name: String)
                             (immediate: Int,
                              zeroPage: Int,
                              zeroPageX: Int,
                              absolute: Int,
                              absoluteX: Int,
                              absoluteY: Int,
                              indirectX: Int,
                              indirectY: Int)(process: (Byte, Readable, CPU) => Int): Seq[Instruction[_ <: HList, _ <: Arg]] = {
    generateNonIndirectXAddressTypes(name)(
      immediate, zeroPage, zeroPageX, absolute, absoluteX
    )(process) ++ Seq(
      Instruction[AbsoluteY :: IndirectX :: IndirectIndexed :: HNil, Address](name, absoluteY, indirectX, indirectY) { (addr, cpu) =>
        process(cpu.memory.read(addr.address), addr, cpu)
      }
    )
  }

  def generateNonIndirectXAddressTypes(name: String)
                             (immediate: Int,
                              zeroPage: Int,
                              zeroPageX: Int,
                              absolute: Int,
                              absoluteX: Int)(process: (Byte, Readable, CPU) => Int): Seq[Instruction[_ <: HList, _ <: Arg]] = {
    generateNonIndirectNoRegisterTypes(name)(immediate, zeroPage, absolute)(process) ++ Seq(
      Instruction[ZeroPageX :: AbsoluteX :: HNil, Address](name, zeroPageX, absoluteX) { (addr, cpu) =>
        process(cpu.memory.read(addr.address), addr, cpu)
      }
    )
  }

  def generateNonIndirectYAddressTypes(name: String)
                                      (immediate: Int,
                                       zeroPage: Int,
                                       zeroPageY: Int,
                                       absolute: Int,
                                       absoluteY: Int)(process: (Byte, Readable, CPU) => Int): Seq[Instruction[_ <: HList, _ <: Arg]] = {
    generateNonIndirectNoRegisterTypes(name)(immediate, zeroPage, absolute)(process) ++ Seq(
      Instruction[ZeroPageY :: AbsoluteY :: HNil, Address](name, zeroPageY, absoluteY) { (addr, cpu) =>
        process(cpu.memory.read(addr.address), addr, cpu)
      }
    )
  }

  def generateNonIndirectNoRegisterTypes(name: String)
                                        (immediate: Int,
                                         zeroPage: Int,
                                         absolute: Int)(process: (Byte, Readable, CPU) => Int): Seq[Instruction[_ <: HList, _ <: Arg]] = {
    Seq(
      Instruction[Immediate :: ZeroPage :: Absolute :: HNil, Readable](name, immediate, zeroPage, absolute) { (addr, cpu) =>
        process(addr.getValue(cpu, cpu.memory), addr, cpu)
      }
    )
  }

  def generateModifyTypes(name: String)
                         (accumulator: Int,
                          zeroPage: Int,
                          zeroPageX: Int,
                          absolute: Int,
                          absoluteX: Int)(process: (Byte, Readable, CPU) => (Byte, Int)): Seq[Instruction[_ <: HList, _ <: Arg]] = {
    generateMemoryModifyTypes(name)(zeroPage, zeroPageX, absolute, absoluteX)(process) ++ Seq(
      Instruction[Accumulator :: HNil, Readable with Writable](name, accumulator) { (addr, cpu) =>
        val value = addr.getValue(cpu, cpu.memory)
        val (ret, cycles) = process(value, addr, cpu)
        addr.writeValue(cpu, cpu.memory, ret)
        cycles
      }
    )
  }

  def generateMemoryModifyTypes(name: String)
                         (zeroPage: Int,
                          zeroPageX: Int,
                          absolute: Int,
                          absoluteX: Int)(process: (Byte, Address, CPU) => (Byte, Int)): Seq[Instruction[_ <: HList, _ <: Arg]] = {
    Seq(
      Instruction[ZeroPage :: ZeroPageX :: Absolute :: AbsoluteX :: HNil, Address]
                 (name, zeroPage, zeroPageX, absolute, absoluteX) { (addr, cpu) =>
        val value = addr.getValue(cpu, cpu.memory)
        val (ret, cycles) = process(value, addr, cpu)
        addr.writeValue(cpu, cpu.memory, ret)
        cycles
      }
    )
  }

  def processNegativeFlag(value: Byte, isNegative: Boolean): Int = {
    if (isNegative) {
      value.toInt
    } else {
      java.lang.Byte.toUnsignedInt(value)
    }
  }

  val cpuInstructionsList: Seq[Instruction[_ <: HList, _ <: Arg]] =
    BranchInstructions.branchInstructions ++
    StoreInstructions.storeInstructions ++
    LoadInstructions.loadInstructions ++
    TransferInstructions.transferInstructions ++
    CompareInstructions.compareInstructions ++
    generateAllAddressTypes("AND")(
      0x29,
      0x25,
      0x35,
      0x2D,
      0x3D,
      0x39,
      0x21,
      0x31
    ) { (value, addr, cpu) =>
      cpu.accumulator = (toUnsignedInt(cpu.accumulator) & toUnsignedInt(value)).toByte
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
    } ++
    generateAllAddressTypes("ORA")(
      0x09,
      0x05,
      0x15,
      0x0D,
      0x1D,
      0x19,
      0x01,
      0x11
    ) { (value, addr, cpu) =>
      cpu.accumulator = (toUnsignedInt(cpu.accumulator) | toUnsignedInt(value)).toByte
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
    } ++
    generateAllAddressTypes("EOR")(
      0x49,
      0x45,
      0x55,
      0x4D,
      0x5D,
      0x59,
      0x41,
      0x51
    ) { (value, addr, cpu) =>
      cpu.accumulator = (toUnsignedInt(cpu.accumulator) ^ toUnsignedInt(value)).toByte
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
    } ++
    generateAllAddressTypes("ADC")(
      0x69,
      0x65,
      0x75,
      0x6D,
      0x7D,
      0x79,
      0x61,
      0x71
    ) { (value, addr, cpu) =>
      val originalAccumulator = cpu.accumulator
      val sum = toUnsignedInt(originalAccumulator) + toUnsignedInt(value) + (if (cpu.carryFlag) 1 else 0)
      cpu.accumulator = sum.toByte
      setZeroNeg(cpu.accumulator, cpu)
      cpu.carryFlag = (sum & 0x100) != 0
      cpu.overflowFlag = (originalAccumulator >= 0 && value >= 0 && cpu.accumulator < 0) ||
        (originalAccumulator < 0 && value < 0 && cpu.accumulator >= 0)
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
    } ++
    generateAllAddressTypes("CMP")(
      0xC9,
      0xC5,
      0xD5,
      0xCD,
      0xDD,
      0xD9,
      0xC1,
      0xD1
    ) { (value, addr, cpu) =>
      val result = toUnsignedInt(cpu.accumulator) - toUnsignedInt(value)
      cpu.carryFlag = (result & 0x100) == 0
      setZeroNeg(result.toByte, cpu)

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
    } ++
    generateAllAddressTypes("SBC")(
      0xE9,
      0xE5,
      0xF5,
      0xED,
      0xFD,
      0xF9,
      0xE1,
      0xF1
    ) { (value, addr, cpu) =>
      val origAcc = toUnsignedInt(cpu.accumulator)
      val subtractedValue = toUnsignedInt(value)
      val sub = origAcc - subtractedValue - (if (cpu.carryFlag) 0 else 1)

      cpu.overflowFlag = ((origAcc ^ sub) & 0x80) != 0 && ((origAcc ^ subtractedValue) & 0x80) != 0

      cpu.accumulator = sub.toByte
      setZeroNeg(cpu.accumulator, cpu)
      cpu.carryFlag = sub >= 0

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
    } ++ generateModifyTypes("LSR")(
      0x4A, 0x46, 0x56, 0x4E, 0x5E
    ) { (value, addr, cpu) =>
      cpu.carryFlag = (value & 1) == 1
      val shifted = (toUnsignedInt(value) >>> 1).toByte
      setZeroNeg(shifted, cpu)

      addr match {
        case _: Accumulator => (shifted, 2)
        case _: ZeroPage => (shifted, 5)
        case _: ZeroPageX => (shifted, 6)
        case _: Absolute => (shifted, 6)
        case _: AbsoluteX => (shifted, 7)
      }
    } ++ generateModifyTypes("ASL")(
      0x0A, 0x06, 0x16, 0x0E, 0x1E
    ) { (value, addr, cpu) =>
      cpu.carryFlag = (toUnsignedInt(value) >> 7) == 1
      val shifted = (toUnsignedInt(value) << 1).toByte
      setZeroNeg(shifted, cpu)

      addr match {
        case _: Accumulator => (shifted, 2)
        case _: ZeroPage => (shifted, 5)
        case _: ZeroPageX => (shifted, 6)
        case _: Absolute => (shifted, 6)
        case _: AbsoluteX => (shifted, 7)
      }
    } ++ generateModifyTypes("ROR")(
      0x6A, 0x66, 0x76, 0x6E, 0x7E
    ) { (value, addr, cpu) =>
      val newCarry = (value & 1) == 1
      val rotated = ((toUnsignedInt(value) >>> 1) | ((if (cpu.carryFlag) 1 else 0) << 7)).toByte
      cpu.carryFlag = newCarry
      setZeroNeg(rotated, cpu)

      addr match {
        case _: Accumulator => (rotated, 2)
        case _: ZeroPage => (rotated, 5)
        case _: ZeroPageX => (rotated, 6)
        case _: Absolute => (rotated, 6)
        case _: AbsoluteX => (rotated, 7)
      }
    } ++ generateModifyTypes("ROL")(
      0x2A, 0x26, 0x36, 0x2E, 0x3E
    ) { (value, addr, cpu) =>
      val newCarry = (toUnsignedInt(value) >> 7) == 1
      val rotated = ((toUnsignedInt(value) << 1) | (if (cpu.carryFlag) 1 else 0)).toByte
      cpu.carryFlag = newCarry

      setZeroNeg(rotated, cpu)

      addr match {
        case _: Accumulator => (rotated, 2)
        case _: ZeroPage => (rotated, 5)
        case _: ZeroPageX => (rotated, 6)
        case _: Absolute => (rotated, 6)
        case _: AbsoluteX => (rotated, 7)
      }
    } ++ generateMemoryModifyTypes("INC")(
      0xE6, 0xF6, 0xEE, 0xFE
    ) { (value, addr, cpu) =>
      val inced = (value + 1).toByte
      setZeroNeg(inced, cpu)

      addr match {
        case _: ZeroPage => (inced, 5)
        case _: ZeroPageX => (inced, 6)
        case _: Absolute => (inced, 6)
        case _: AbsoluteX => (inced, 7)
      }
    } ++ generateMemoryModifyTypes("DEC")(
      0xC6, 0xD6, 0xCE, 0xDE
    ) { (value, addr, cpu) =>
      val deced = (value - 1).toByte
      setZeroNeg(deced, cpu)

      addr match {
        case _: ZeroPage => (deced, 5)
        case _: ZeroPageX => (deced, 6)
        case _: Absolute => (deced, 6)
        case _: AbsoluteX => (deced, 7)
      }
    } ++ Seq[Instruction[_ <: HList, _ <: Arg]](
      Instruction[ZeroPage :: Absolute :: HNil, Address]("BIT", 0x24, 0x2C) { (addr, cpu) =>
        val memoryValue = cpu.memory.read(addr.address)
        val masked = toUnsignedInt(memoryValue) & toUnsignedInt(cpu.accumulator)
        cpu.zeroFlag = masked == 0
        cpu.overflowFlag = (memoryValue & 0x40) != 0
        cpu.negativeFlag = memoryValue < 0

        addr match {
          case _: ZeroPage => 3
          case _: Absolute => 4
        }
      },

      Instruction[NoArgs :: HNil, Arg]("CLC", 0x18) { (_, cpu) =>
        cpu.carryFlag = false
        2
      },

      Instruction[NoArgs :: HNil, Arg]("CLD", 0xD8) { case (_, cpu) =>
        cpu.decimalMode = false
        2
      },

      Instruction[NoArgs :: HNil, Arg]("CLI", 0x58) { case (_, cpu) =>
        cpu.interruptDisable = false
        2
      },

      Instruction[NoArgs :: HNil, Arg]("CLV", 0xB8) { case (_, cpu) =>
        cpu.overflowFlag = false
        2
      },

      Instruction[NoArgs :: HNil, Arg]("DEX", 0xCA) { (_, cpu) =>
        cpu.xRegister = (cpu.xRegister - 1).toByte
        cpu.zeroFlag = cpu.xRegister == 0
        cpu.negativeFlag = cpu.xRegister < 0
        2
      },

      Instruction[NoArgs :: HNil, Arg]("DEY", 0x88) { (_, cpu) =>
        cpu.yRegister = (cpu.yRegister - 1).toByte
        cpu.zeroFlag = cpu.yRegister == 0
        cpu.negativeFlag = cpu.yRegister < 0
        2
      },

      Instruction[NoArgs :: HNil, Arg]("INX", 0xE8) { (_, cpu) =>
        cpu.xRegister = (cpu.xRegister + 1).toByte
        cpu.zeroFlag = cpu.xRegister == 0
        cpu.negativeFlag = cpu.xRegister < 0
        2
      },

      Instruction[NoArgs :: HNil, Arg]("INY", 0xC8) { (_, cpu) =>
        cpu.yRegister = (cpu.yRegister + 1).toByte
        cpu.zeroFlag = cpu.yRegister == 0
        cpu.negativeFlag = cpu.yRegister < 0
        2
      },

      Instruction[Absolute :: Indirect :: HNil, Address]("JMP", 0x4C, 0x6C) { (addr, cpu) =>
        cpu.programCounter = addr.address

        addr match {
          case _: Absolute => 3
          case _: Indirect => 5
        }
      },

      Instruction[Absolute :: HNil, Absolute]("JSR", 0x20) { (addr, cpu) =>
        val addressToPush = cpu.programCounter - 1 // already incremented by CPU
        pushToStack((addressToPush >> 8).toByte, cpu)
        pushToStack((addressToPush & 0xFF).toByte, cpu)

        cpu.programCounter = addr.address

        6
      },

      Instruction[Immediate :: Absolute :: IndirectX :: Relative :: NoArgs :: HNil, Arg]("NOP",
        Seq(0x04, 0x44, 0x64),
        Seq(0x0C),
        Seq(0x14, 0x34, 0x54, 0x74, 0xD4, 0xF4),
        Seq(0x80),
        Seq(0x1A, 0x3A, 0x5A, 0x7A, 0xDA, 0xEA, 0xFA)
      ) { (addr, _) =>
        addr match {
          case _: Immediate => 3
          case _: Absolute => 4
          case _: IndirectX => 4
          case _: Relative => 2
          case _: NoArgs => 2
        }
      },

      Instruction[NoArgs :: HNil, Arg]("PHA", 0x48) { (_, cpu) =>
        pushToStack(cpu.accumulator, cpu)
        3
      },

      Instruction[NoArgs :: HNil, Arg]("PHP", 0x08) { (_, cpu) =>
        pushToStack(cpu.statusRegister, cpu)

        3
      },

      Instruction[NoArgs :: HNil, Arg]("PLP", 0x28) { (_, cpu) =>
        val stackValue = toUnsignedInt(popFromStack(cpu))

        def atIndex(index: Int): Boolean = {
          ((stackValue >> index) & 1) == 1
        }

        cpu.carryFlag = atIndex(0)
        cpu.zeroFlag = atIndex(1)
        cpu.interruptDisable = atIndex(2)
        cpu.decimalMode = atIndex(3)
        cpu.overflowFlag = atIndex(6)
        cpu.negativeFlag = atIndex(7)

        4
      },

      Instruction[NoArgs :: HNil, Arg]("PLA", 0x68) { (_, cpu) =>
        cpu.accumulator = popFromStack(cpu)
        setZeroNeg(cpu.accumulator, cpu)
        4
      },

      Instruction[NoArgs :: HNil, Arg]("RTS", 0x60) { (_, cpu) =>
        val lowerByte = popFromStack(cpu)
        val upperByte = popFromStack(cpu)

        cpu.programCounter = (java.lang.Byte.toUnsignedInt(lowerByte) | (java.lang.Byte.toUnsignedInt(upperByte) << 8)) + 1

        6
      },

      Instruction[NoArgs :: HNil, Arg]("RTI", 0x40) { (_, cpu) =>
        val stackValue = toUnsignedInt(popFromStack(cpu)) // pop cpu status
        def atIndex(index: Int): Boolean = {
          ((stackValue >> index) & 1) == 1
        }

        cpu.carryFlag = atIndex(0)
        cpu.zeroFlag = atIndex(1)
        cpu.interruptDisable = atIndex(2)
        cpu.decimalMode = atIndex(3)
        cpu.overflowFlag = atIndex(6)
        cpu.negativeFlag = atIndex(7)

        val lowerByte = popFromStack(cpu)
        val upperByte = popFromStack(cpu)

        cpu.programCounter = java.lang.Byte.toUnsignedInt(lowerByte) | (java.lang.Byte.toUnsignedInt(upperByte) << 8)

        6
      },

      Instruction[NoArgs :: HNil, Arg]("SEC", 0x38) { (_, cpu) =>
        cpu.carryFlag = true
        2
      },

      Instruction[NoArgs :: HNil, Arg]("SED", 0xF8) { (_, cpu) =>
        cpu.decimalMode = true
        2
      },

      Instruction[NoArgs :: HNil, Arg]("SEI", 0x78) { (_, cpu) =>
        cpu.interruptDisable = true
        2
      }
    )

  val cpuInstructions = cpuInstructionsList.flatMap(i => i.opcodesToArgs.map(a => a._1 -> (i -> a._2)))
    .asInstanceOf[Seq[(Byte, (Instruction[Nothing, Nothing], ArgParser[Nothing]))]].toMap
}
