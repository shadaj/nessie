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
  val size: Int
  def parse(getArg: Int => Byte, cpu: CPU): Args
}

object ArgsParser {
  implicit val forNil: ArgsParser[HNil] = new ArgsParser[HNil] {
    override val size: Int = 0
    override def parse(getArg: Int => Byte, cpu: CPU): HNil = HNil
  }

  implicit def forCons[Head <: Arg, Tail <: HList](implicit argParser: ArgParser[Head], tailParser: ArgsParser[Tail]): ArgsParser[Head :: Tail] = new ArgsParser[Head :: Tail] {
    override val size: Int = argParser.size + tailParser.size
    override def parse(getArg: Int => Byte, cpu: CPU): Head :: Tail = {
      argParser.parse(getArg, cpu) :: tailParser.parse(i => getArg(i + argParser.size), cpu)
    }
  }
}

case class Instruction[Args <: HList](opcode: Int, name: String)(execute: (Args, CPU) => Int)(implicit parseArgs: ArgsParser[Args], argsToList: ToList[Args, Arg]) {
  val argsSize = parseArgs.size
  def parseArgs(argsBytes: Seq[Byte], cpu: CPU): List[Arg] = parseArgs.parse(argsBytes, cpu).toList
  def run(getArg: Int => Byte, log: Boolean = false)(cpu: CPU) = {
    val parsedArgs = parseArgs.parse(getArg, cpu)
    if (log) {
      println(s"$this ${parsedArgs.toList.mkString(" ")}")
    }
    execute(parsedArgs, cpu)
  }

  override def toString: String = name
}

object Instruction {
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
                              indirectY: Int)(process: (Byte, Address, CPU) => Int): Seq[Instruction[_ <: HList]] = {
    generateNonIndirectXAddressTypes(name)(
      immediate, zeroPage, zeroPageX, absolute, absoluteX
    )(process) ++ Seq(
      Instruction[AbsoluteY :: HNil](absoluteY, name) { case (addr :: HNil, cpu) =>
        process(cpu.memory.read(addr.address), addr, cpu)
      },
      Instruction[IndirectX :: HNil](indirectX, name) { case (addr :: HNil, cpu) =>
        process(cpu.memory.read(addr.address), addr, cpu)
      },
      Instruction[IndirectIndexed :: HNil](indirectY, name) { case (addr :: HNil, cpu) =>
        process(cpu.memory.read(addr.address), addr, cpu)
      }
    )
  }

  def generateNonIndirectXAddressTypes(name: String)
                             (immediate: Int,
                              zeroPage: Int,
                              zeroPageX: Int,
                              absolute: Int,
                              absoluteX: Int)(process: (Byte, Address, CPU) => Int): Seq[Instruction[_ <: HList]] = {
    generateNonIndirectNoRegisterTypes(name)(immediate, zeroPage, absolute)(process) ++ Seq(
      Instruction[ZeroPageX :: HNil](zeroPageX, name) { case (addr :: HNil, cpu) =>
        process(cpu.memory.read(addr.address), addr, cpu)
      },
      Instruction[AbsoluteX :: HNil](absoluteX, name) { case (addr :: HNil, cpu) =>
        process(cpu.memory.read(addr.address), addr, cpu)
      }
    )
  }

  def generateNonIndirectNoRegisterTypes(name: String)
                                        (immediate: Int,
                                         zeroPage: Int,
                                         absolute: Int)(process: (Byte, Address, CPU) => Int): Seq[Instruction[_ <: HList]] = {
    Seq(
      Instruction[Immediate :: HNil](immediate, name) { case (addr :: HNil, cpu) =>
        process(addr.constant, addr, cpu)
      },
      Instruction[ZeroPage :: HNil](zeroPage, name) { case (addr :: HNil, cpu) =>
        process(cpu.memory.read(addr.address), addr, cpu)
      },
      Instruction[Absolute :: HNil](absolute, name) { case (addr :: HNil, cpu) =>
        process(cpu.memory.read(addr.twoBytes), addr, cpu)
      }
    )
  }

  def generateModifyTypes(name: String)
                         (accumulator: Int,
                          zeroPage: Int,
                          zeroPageX: Int,
                          absolute: Int,
                          absoluteX: Int)(process: (Byte, Address, CPU) => (Byte, Int)): Seq[Instruction[_ <: HList]] = {
    Seq(
      Instruction[HNil](accumulator, name) { (_, cpu) =>
        val value = cpu.accumulator
        val (ret, cycles) = process(value, Accumulator, cpu)
        cpu.accumulator = ret
        cycles
      },
      Instruction[ZeroPage :: HNil](zeroPage, name) { case (addr :: HNil, cpu) =>
        val value = cpu.memory.read(addr.address)
        val (ret, cycles) = process(value, addr, cpu)
        cpu.memory.write(addr.address, ret)
        cycles
      },
      Instruction[ZeroPageX :: HNil](zeroPageX, name) { case (addr :: HNil, cpu) =>
        val value = cpu.memory.read(addr.address)
        val (ret, cycles) = process(value, addr, cpu)
        cpu.memory.write(addr.address, ret)
        cycles
      },
      Instruction[Absolute :: HNil](absolute, name) { case (addr :: HNil, cpu) =>
        val value = cpu.memory.read(addr.address)
        val (ret, cycles) = process(value, addr, cpu)
        cpu.memory.write(addr.address, ret)
        cycles
      },
      Instruction[AbsoluteX :: HNil](absoluteX, name) { case (addr :: HNil, cpu) =>
        val value = cpu.memory.read(addr.address)
        val (ret, cycles) = process(value, addr, cpu)
        cpu.memory.write(addr.address, ret)
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

  val cpuInstructionsList: Seq[Instruction[_ <: HList]] =
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
      cpu.carryFlag = (sum >> 8) != 0
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
      cpu.carryFlag = toUnsignedInt(cpu.accumulator) >= toUnsignedInt(value)
      cpu.zeroFlag = toUnsignedInt(cpu.accumulator) == toUnsignedInt(value)
      cpu.negativeFlag = processNegativeFlag(cpu.accumulator, value <= 0) < value

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
        case Accumulator => (shifted, 2)
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
        case Accumulator => (shifted, 2)
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
        case Accumulator => (rotated, 2)
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
        case Accumulator => (rotated, 2)
        case _: ZeroPage => (rotated, 5)
        case _: ZeroPageX => (rotated, 6)
        case _: Absolute => (rotated, 6)
        case _: AbsoluteX => (rotated, 7)
      }
    } ++ Seq(
      Instruction[ZeroPage :: HNil](0x24, "BIT") { case (addr :: HNil, cpu) =>
        val memoryValue = cpu.memory.read(addr.address)
        val masked = toUnsignedInt(memoryValue) & toUnsignedInt(cpu.accumulator)
        cpu.zeroFlag = masked == 0
        cpu.overflowFlag = (memoryValue << 1).toByte < 0
        cpu.negativeFlag = memoryValue < 0
        4
      },

      Instruction[Absolute :: HNil](0x2C, "BIT") { case (Absolute(addr) :: HNil, cpu) =>
        val memoryValue = cpu.memory.read(addr)
        val masked = toUnsignedInt(memoryValue) & toUnsignedInt(cpu.accumulator)
        cpu.zeroFlag = masked == 0
        cpu.overflowFlag = (memoryValue << 1).toByte < 0 // TODO check this logic
        cpu.negativeFlag = memoryValue < 0
        4
      },

      Instruction[HNil](0x18, "CLC") { (_, cpu) =>
        cpu.carryFlag = false
        2
      },

      Instruction[HNil](0xD8, "CLD") { case (_, cpu) =>
        cpu.decimalMode = false
        2
      },

      Instruction[HNil](0x58, "CLI") { case (_, cpu) =>
        cpu.interruptDisable = false
        2
      },

      Instruction[HNil](0xB8, "CLV") { case (_, cpu) =>
        cpu.overflowFlag = false
        2
      },

      Instruction[HNil](0xCA, "DEX") { (_, cpu) =>
        cpu.xRegister = (cpu.xRegister - 1).toByte
        cpu.zeroFlag = cpu.xRegister == 0
        cpu.negativeFlag = cpu.xRegister < 0
        2
      },

      Instruction[HNil](0x88, "DEY") { (_, cpu) =>
        cpu.yRegister = (cpu.yRegister - 1).toByte
        cpu.zeroFlag = cpu.yRegister == 0
        cpu.negativeFlag = cpu.yRegister < 0
        2
      },

      Instruction[ZeroPage :: HNil](0xE6, "INC") { case (ZeroPage(address) :: HNil, cpu) =>
        val newValue = (cpu.memory.read(address) + 1).toByte

        cpu.zeroFlag = newValue == 0
        cpu.negativeFlag = newValue < 0

        cpu.memory.write(address, newValue)

        5
      },

      Instruction[HNil](0xE8, "INX") { (_, cpu) =>
        cpu.xRegister = (cpu.xRegister + 1).toByte
        cpu.zeroFlag = cpu.xRegister == 0
        cpu.negativeFlag = cpu.xRegister < 0
        2
      },

      Instruction[HNil](0xC8, "INY") { (_, cpu) =>
        cpu.yRegister = (cpu.yRegister + 1).toByte
        cpu.zeroFlag = cpu.yRegister == 0
        cpu.negativeFlag = cpu.yRegister < 0
        2
      },

      Instruction[Absolute :: HNil](0x4C, "JMP") { case (Absolute(address) :: HNil, cpu) =>
        cpu.programCounter = address
        3
      },

      Instruction[Absolute :: HNil](0x20, "JSR") { case (Absolute(address) :: HNil, cpu) =>
        val addressToPush = cpu.programCounter - 1 // already incremented by CPU
        pushToStack((addressToPush >> 8).toByte, cpu)
        pushToStack((addressToPush & 0xFF).toByte, cpu)

        cpu.programCounter = address

        6
      },

      Instruction[HNil](0xEA, "NOP") { (_, _) =>
        2
      },

      Instruction[HNil](0x48, "PHA") { (_, cpu) =>
        pushToStack(cpu.accumulator, cpu)
        3
      },

      Instruction[HNil](0x08, "PHP") { (_, cpu) =>
        pushToStack(cpu.statusRegister, cpu)

        3
      },

      Instruction[HNil](0x28, "PLP") { (_, cpu) =>
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

      Instruction[HNil](0x68, "PLA") { (_, cpu) =>
        cpu.accumulator = popFromStack(cpu)
        setZeroNeg(cpu.accumulator, cpu)
        4
      },

      Instruction[HNil](0x60, "RTS") { (_, cpu) =>
        val lowerByte = popFromStack(cpu)
        val upperByte = popFromStack(cpu)

        cpu.programCounter = (java.lang.Byte.toUnsignedInt(lowerByte) | (java.lang.Byte.toUnsignedInt(upperByte) << 8)) + 1

        6
      },

      Instruction[HNil](0x40, "RTI") { (_, cpu) =>
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

      Instruction[HNil](0x38, "SEC") { (_, cpu) =>
        cpu.carryFlag = true
        2
      },

      Instruction[HNil](0xF8, "SED") { (_, cpu) =>
        cpu.decimalMode = true
        2
      },

      Instruction[HNil](0x78, "SEI") { (_, cpu) =>
        cpu.interruptDisable = true
        2
      }
    )

  val cpuInstructions = cpuInstructionsList.map(i => i.opcode.toByte -> i)
    .asInstanceOf[Seq[(Byte, Instruction[Nothing])]].toMap
}
