package me.shadaj.nessie

import shapeless._
import java.lang.Byte.toUnsignedInt

import me.shadaj.nessie.instructions._

import scala.annotation.unchecked.uncheckedVariance

trait Arg

trait ArgParser[+A <: Arg] {
  val size: Int
  def parse(getArg: Int => Byte, cpu: CPU): A
}

trait ArgsParser[Args <: HList] {
  val parsers: Seq[ArgParser[Arg]]
}

object ArgsParser {
  implicit val forNil: ArgsParser[HNil] = new ArgsParser[HNil] {
    override val parsers: Seq[ArgParser[Arg]] = Seq.empty
  }

  implicit def forCons[Head <: Arg, Tail <: HList](implicit argParser: ArgParser[Head], tailParser: ArgsParser[Tail]): ArgsParser[Head :: Tail] = new ArgsParser[Head :: Tail] {
    override val parsers: Seq[ArgParser[Arg]] = argParser +: tailParser.parsers
  }
}

case class Instruction[+Args <: HList, +LubA <: Arg](name: String, opcodes: Seq[Int]*)
                                                   (execute: (LubA, CPU) => Int)
                                                   (implicit parseArgs: ArgsParser[Args]) {
  if (opcodes.size != parseArgs.parsers.size) {
    throw new IllegalArgumentException("Opcodes and parsers must be the same size")
  }

  val opcodesToArgs: Seq[(Byte, ArgParser[Arg])] =
    opcodes.zip(parseArgs.parsers).flatMap(t => t._1.map(o => o.toByte -> t._2))

  def run(parsedArg: LubA @uncheckedVariance, log: Boolean = false)(cpu: CPU): Int = {
    val cycles = execute(parsedArg, cpu)

    if (log) {
      println(s"$this $parsedArg $cycles A:${cpu.accumulator} X:${cpu.xRegister}")
    }

    cycles
  }

  override def toString: String = name
}

class SeparateApply
object SeparateApply {
  implicit val imp: SeparateApply = new SeparateApply
}

object Instruction {
  def apply[Args <: HList, LubA <: Arg](name: String, opcodes: Int*)
                                       (execute: (LubA, CPU) => Int)
                                       (implicit parseArgs: ArgsParser[Args], separate: SeparateApply): Instruction[Args, LubA] = {
    apply[Args, LubA](name, opcodes.map(Seq(_)): _*)(execute)(parseArgs)
  }

  def pageCrossExtra(original: Int, shifted: Int): Int = {
    if ((original & 0xFF00) != (shifted & 0xFF00)) 1 else 0
  }

  def setZeroNeg(value: Byte, cpu: CPU): Unit = {
    cpu.zeroFlag = value == 0
    cpu.negativeFlag = value < 0
  }

  def pushToStack(value: Byte, cpu: CPU): Unit = {
    cpu.memory.write(0x0100 | toUnsignedInt(cpu.stackPointer), value)
    cpu.stackPointer = (cpu.stackPointer - 1).toByte
  }

  def popFromStack(cpu: CPU): Byte = {
    cpu.stackPointer = (cpu.stackPointer + 1).toByte
    val ret = cpu.memory.read(0x0100 | toUnsignedInt(cpu.stackPointer))
    ret
  }

  type AllAddressTypes =
    Immediate :: ZeroPage :: ZeroPageX :: Absolute :: AbsoluteX :: AbsoluteY :: IndirectX :: IndirectIndexed :: HNil

  type ModifyTypes = Accumulator :: ZeroPage :: ZeroPageX :: Absolute :: AbsoluteX :: HNil

  val cpuInstructionsList: Seq[Instruction[HList, Arg]] =
    BranchInstructions.branchInstructions ++
    StoreInstructions.storeInstructions ++
    LoadInstructions.loadInstructions ++
    TransferInstructions.transferInstructions ++
    CompareInstructions.compareInstructions ++
    Seq[Instruction[_ <: HList, _ <: Arg]](
      Instruction[Immediate :: HNil, Arg]("BRK", 0x00) { (addr, cpu) =>
        cpu.interruptAddress = Some(cpu.memory.readTwoBytes(0xFFFE))
        0
      },
      Instruction[AllAddressTypes, Readable]("ADC",
        0x69, 0x65, 0x75, 0x6D, 0x7D, 0x79, 0x61, 0x71
      ) { (addr, cpu) =>
        val value = addr.getValue(cpu, cpu.memory)
        val sum = toUnsignedInt(cpu.accumulator) + toUnsignedInt(value) + (if (cpu.carryFlag) 1 else 0)

        cpu.carryFlag = (sum & 0x100) != 0
        cpu.overflowFlag = ((cpu.accumulator.toInt + value.toInt) > 0) != ((cpu.accumulator.toInt + value.toInt).toByte > 0)

        cpu.accumulator = sum.toByte
        setZeroNeg(cpu.accumulator, cpu)

        addr match {
          case _: Immediate => 2
          case _: ZeroPage => 3
          case _: ZeroPageX => 4
          case _: Absolute => 4
          case a: AbsoluteX => 4 + pageCrossExtra(a.absolute, a.address)
          case _: AbsoluteY => 4
          case _: IndirectX => 6
          case _: IndirectIndexed => 5
        }
      },
      Instruction[AllAddressTypes, Readable]("AND",
        0x29, 0x25, 0x35, 0x2D, 0x3D, 0x39, 0x21, 0x31
      ) { (addr, cpu) =>
        cpu.accumulator = (toUnsignedInt(cpu.accumulator) &
          toUnsignedInt(addr.getValue(cpu, cpu.memory))).toByte
        setZeroNeg(cpu.accumulator, cpu)
        addr match {
          case _: Immediate => 2
          case _: ZeroPage => 3
          case _: ZeroPageX => 4
          case _: Absolute => 4
          case a: AbsoluteX => 4 + pageCrossExtra(a.absolute, a.address)
          case _: AbsoluteY => 4
          case _: IndirectX => 6
          case _: IndirectIndexed => 5
        }
      },
      Instruction[ModifyTypes, Readable with Writable]("ASL",
        0x0A, 0x06, 0x16, 0x0E, 0x1E
      ) { (addr, cpu) =>
        val value = addr.getValue(cpu, cpu.memory)
        cpu.carryFlag = (toUnsignedInt(value) >> 7) == 1
        val shifted = (toUnsignedInt(value) << 1).toByte
        setZeroNeg(shifted, cpu)

        addr.writeValue(cpu, cpu.memory, shifted)
        addr match {
          case _: Accumulator => 2
          case _: ZeroPage => 5
          case _: ZeroPageX => 6
          case _: Absolute => 6
          case _: AbsoluteX => 7
        }
      },

      Instruction[ZeroPage :: ZeroPageX :: Absolute :: AbsoluteX :: HNil, Address]("DEC",
        0xC6, 0xD6, 0xCE, 0xDE
      ) { (addr, cpu) =>
        val inced = (addr.getValue(cpu, cpu.memory) - 1).toByte
        setZeroNeg(inced, cpu)
        addr.writeValue(cpu, cpu.memory, inced)

        addr match {
          case _: ZeroPage => 5
          case _: ZeroPageX => 6
          case _: Absolute => 6
          case _: AbsoluteX => 7
        }
      },

      Instruction[AllAddressTypes, Readable]("EOR",
        0x49, 0x45, 0x55, 0x4D, 0x5D, 0x59, 0x41, 0x51
      ) { (addr, cpu) =>
        val value = addr.getValue(cpu, cpu.memory)
        cpu.accumulator = (toUnsignedInt(cpu.accumulator) ^ toUnsignedInt(value)).toByte
        setZeroNeg(cpu.accumulator, cpu)
        addr match {
          case _: Immediate => 2
          case _: ZeroPage => 3
          case _: ZeroPageX => 4
          case _: Absolute => 4
          case a: AbsoluteX => 4 + pageCrossExtra(a.absolute, a.address)
          case _: AbsoluteY => 4
          case _: IndirectX => 6
          case _: IndirectIndexed => 5
        }
      },

      Instruction[ZeroPage :: ZeroPageX :: Absolute :: AbsoluteX :: HNil, Address]("INC",
        0xE6, 0xF6, 0xEE, 0xFE
      ) { (addr, cpu) =>
        val inced = (addr.getValue(cpu, cpu.memory) + 1).toByte
        setZeroNeg(inced, cpu)
        addr.writeValue(cpu, cpu.memory, inced)

        addr match {
          case _: ZeroPage => 5
          case _: ZeroPageX => 6
          case _: Absolute => 6
          case _: AbsoluteX => 7
        }
      },
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

      Instruction[ModifyTypes, Readable with Writable]("LSR",
        0x4A, 0x46, 0x56, 0x4E, 0x5E
      ) { (addr, cpu) =>
        val value = addr.getValue(cpu, cpu.memory)
        cpu.carryFlag = (value & 1) == 1
        val shifted = (toUnsignedInt(value) >>> 1).toByte
        setZeroNeg(shifted, cpu)

        addr.writeValue(cpu, cpu.memory, shifted)
        addr match {
          case _: Accumulator => 2
          case _: ZeroPage => 5
          case _: ZeroPageX => 6
          case _: Absolute => 6
          case _: AbsoluteX => 7
        }
      },

      Instruction[Immediate :: Absolute :: AbsoluteX :: IndirectX :: Relative :: NoArgs :: HNil, Arg]("NOP",
        Seq(0x04, 0x44, 0x64),
        Seq(0x0C),
        Seq(0x1C, 0x3C, 0x5C, 0x7C, 0xDC, 0xFC),
        Seq(0x14, 0x34, 0x54, 0x74, 0xD4, 0xF4),
        Seq(0x80),
        Seq(0x1A, 0x3A, 0x5A, 0x7A, 0xDA, 0xEA, 0xFA)
      ) { (addr, _) =>
        addr match {
          case _: Immediate => 3
          case _: Absolute => 4
          case a: AbsoluteX => 4 + pageCrossExtra(a.absolute, a.address)
          case _: IndirectX => 4
          case _: Relative => 2
          case _: NoArgs => 2
        }
      },

      Instruction[AllAddressTypes, Readable]("ORA",
        0x09, 0x05, 0x15, 0x0D, 0x1D, 0x19, 0x01, 0x11
      ) { (addr, cpu) =>
        val value = addr.getValue(cpu, cpu.memory)
        cpu.accumulator = (toUnsignedInt(cpu.accumulator) | toUnsignedInt(value)).toByte
        setZeroNeg(cpu.accumulator, cpu)
        addr match {
          case _: Immediate => 2
          case _: ZeroPage => 3
          case _: ZeroPageX => 4
          case _: Absolute => 4
          case a: AbsoluteX => 4 + pageCrossExtra(a.absolute, a.address)
          case _: AbsoluteY => 4
          case _: IndirectX => 6
          case _: IndirectIndexed => 5
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

      Instruction[ModifyTypes, Readable with Writable]("ROL",
        0x2A, 0x26, 0x36, 0x2E, 0x3E
      ) { (addr, cpu) =>
        val value = addr.getValue(cpu, cpu.memory)
        val newCarry = (toUnsignedInt(value) >> 7) == 1
        val rotated = ((toUnsignedInt(value) << 1) | (if (cpu.carryFlag) 1 else 0)).toByte
        cpu.carryFlag = newCarry

        setZeroNeg(rotated, cpu)

        addr.writeValue(cpu, cpu.memory, rotated)
        addr match {
          case _: Accumulator => 2
          case _: ZeroPage => 5
          case _: ZeroPageX => 6
          case _: Absolute => 6
          case _: AbsoluteX => 7
        }
      },

      Instruction[ModifyTypes, Readable with Writable]("ROR",
        0x6A, 0x66, 0x76, 0x6E, 0x7E
      ) { (addr, cpu) =>
        val value = addr.getValue(cpu, cpu.memory)
        val newCarry = (value & 1) == 1
        val rotated = ((toUnsignedInt(value) >>> 1) | ((if (cpu.carryFlag) 1 else 0) << 7)).toByte
        cpu.carryFlag = newCarry
        setZeroNeg(rotated, cpu)

        addr.writeValue(cpu, cpu.memory, rotated)
        addr match {
          case _: Accumulator => 2
          case _: ZeroPage => 5
          case _: ZeroPageX => 6
          case _: Absolute => 6
          case _: AbsoluteX => 7
        }
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

      Instruction[AllAddressTypes, Readable]("SBC",
        0xE9, 0xE5, 0xF5, 0xED, 0xFD, 0xF9, 0xE1, 0xF1
      ) { (addr, cpu) =>
        val value = addr.getValue(cpu, cpu.memory)
        val subtractedValue = toUnsignedInt(value)
        val sub = toUnsignedInt(cpu.accumulator) - subtractedValue - (if (cpu.carryFlag) 0 else 1)

        cpu.carryFlag = (sub & 0x100) == 0
        cpu.overflowFlag = ((cpu.accumulator.toInt - value.toInt) > 0) != ((cpu.accumulator.toInt - value.toInt).toByte > 0)

        cpu.accumulator = sub.toByte
        setZeroNeg(cpu.accumulator, cpu)

        addr match {
          case _: Immediate => 2
          case _: ZeroPage => 3
          case _: ZeroPageX => 4
          case _: Absolute => 4
          case a: AbsoluteX => 4 + pageCrossExtra(a.absolute, a.address)
          case _: AbsoluteY => 4
          case _: IndirectX => 6
          case _: IndirectIndexed => 5
        }
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

  val cpuInstructions: Map[Byte, (Instruction[HList, Arg], ArgParser[Arg])] = {
    cpuInstructionsList.flatMap(i => i.opcodesToArgs.map(a => a._1 -> (i, a._2)))
      .toMap
  }
}
