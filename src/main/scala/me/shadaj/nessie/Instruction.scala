package me.shadaj.nessie

import shapeless._
import shapeless.ops.hlist.ToList
import java.lang.Byte.toUnsignedInt

sealed trait Arg

trait ArgParser[A <: Arg] {
  val size: Int
  def parse(getArg: Int => Byte, cpu: CPU): A
}

sealed trait Address extends Arg

case class Immediate(constant: Byte) extends Address {
  override def toString: String = f"#${"$"}$constant%x | #$constant"
}

object Immediate {
  implicit val parser: ArgParser[Immediate] = new ArgParser[Immediate] {
    override val size: Int = 1

    override def parse(getArg: Int => Byte, cpu: CPU): Immediate = Immediate(getArg(0))
  }
}

case class ZeroPage(address: Int) extends Address {
  override def toString: String = address.formatted("$%x".toUpperCase)
}

object ZeroPage {
  implicit val parser: ArgParser[ZeroPage] = new ArgParser[ZeroPage] {
    override val size: Int = 1

    override def parse(getArg: Int => Byte, cpu: CPU): ZeroPage = {
      ZeroPage(java.lang.Byte.toUnsignedInt(getArg(0)))
    }
  }
}

case class ZeroPageX(indirect: Int, x: Int) extends Address {
  val address = (indirect + x) & 0xFF // wraps around
  override def toString: String = f"${"$"}$indirect%x,$x (ind,x)"
}

object ZeroPageX {
  implicit val parser: ArgParser[ZeroPageX] = new ArgParser[ZeroPageX] {
    override val size: Int = 1

    override def parse(getArg: Int => Byte, cpu: CPU): ZeroPageX = {
      ZeroPageX(
        java.lang.Byte.toUnsignedInt(getArg(0)),
        toUnsignedInt(cpu.xRegister)
      )
    }
  }
}

case class Absolute(twoBytes: Int) extends Address {
  override def toString: String = twoBytes.formatted("$%x".toUpperCase)
}

object Absolute {
  implicit val parser: ArgParser[Absolute] = new ArgParser[Absolute] {
    override val size: Int = 2

    override def parse(getArg: Int => Byte, cpu: CPU): Absolute = {
      import java.lang.Byte.toUnsignedInt
      Absolute(
        toUnsignedInt(getArg(0)) | (toUnsignedInt(getArg(1)) << 8)
      )
    }
  }
}

case class AbsoluteX(absolute: Int, x: Int) extends Address {
  val address = absolute + x
  override def toString: String = f"${"$"}$absolute%x,$x (abs,x)"
}

object AbsoluteX {
  implicit val parser: ArgParser[AbsoluteX] = new ArgParser[AbsoluteX] {
    override val size: Int = 2

    override def parse(getArg: Int => Byte, cpu: CPU): AbsoluteX = {
      import java.lang.Byte.toUnsignedInt
      AbsoluteX(
        toUnsignedInt(getArg(0)) | (toUnsignedInt(getArg(1)) << 8),
        toUnsignedInt(cpu.xRegister)
      )
    }
  }
}

case class IndirectIndexed(indirect: Int, y: Int) extends Address {
  val address = indirect + y
  override def toString: String = f"${"$"}$indirect%x,$y (ind,y)"
}

object IndirectIndexed {
  implicit val parser: ArgParser[IndirectIndexed] = new ArgParser[IndirectIndexed] {
    override val size: Int = 1

    override def parse(getArg: Int => Byte, cpu: CPU): IndirectIndexed = {
      val zeroPageAddress = cpu.memory.readTwoBytes(java.lang.Byte.toUnsignedInt(getArg(0)))

      IndirectIndexed(zeroPageAddress, toUnsignedInt(cpu.yRegister))
    }
  }
}

case class Relative(relativeAddress: Byte) extends Address {
  override def toString: String = s"*${if (relativeAddress >= 0) "+" else ""}$relativeAddress"
}

object Relative {
  implicit val parser: ArgParser[Relative] = new ArgParser[Relative] {
    override val size: Int = 1

    override def parse(getArg: Int => Byte, cpu: CPU): Relative = {
      Relative(getArg(0))
    }
  }
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
  val myArguments: Immediate :: HNil = Immediate(1) :: HNil

  def setZeroNeg(value: Byte, cpu: CPU) = {
    cpu.zeroFlag = value == 0
    cpu.negativeFlag = value < 0
  }

  def pushToStack(value: Byte, cpu: CPU) = {
    cpu.memory.write(0x01FF & cpu.stackPointer, value)
    cpu.stackPointer = (cpu.stackPointer - 1).toByte
  }

  def popFromStack(cpu: CPU): Byte = {
    cpu.stackPointer = (cpu.stackPointer + 1).toByte
    val ret = cpu.memory.read(0x01FF & cpu.stackPointer)
    cpu.memory.write(0x01FF & cpu.stackPointer, 0)
    ret
  }

  val cpuInstructions: Map[Byte, Instruction[_]] = Seq(
    Instruction[Immediate :: HNil](0x69, "ADC") { case (Immediate(const) :: HNil, cpu) =>
      val sum = toUnsignedInt(cpu.accumulator) + toUnsignedInt(const)
      cpu.accumulator = sum.toByte
      setZeroNeg(cpu.accumulator, cpu)
      cpu.overflowFlag = (sum >> 8) != 0

      2
    },

    Instruction[Immediate :: HNil](0x29, "AND") { case (Immediate(const) :: HNil, cpu) =>
      cpu.accumulator = (toUnsignedInt(cpu.accumulator) & toUnsignedInt(const)).toByte
      setZeroNeg(cpu.accumulator, cpu)

      2
    },

    Instruction[HNil](0x0A, "ASL") { (_, cpu) =>
      cpu.carryFlag = (cpu.accumulator >> 7) == 1
      cpu.accumulator = (cpu.accumulator << 1).toByte
      setZeroNeg(cpu.accumulator, cpu)

      2
    },

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

    Instruction[Relative :: HNil](0x30, "BMI") { case (Relative(relativeAddr) :: HNil, cpu) =>
      if (cpu.negativeFlag) {
        cpu.programCounter += relativeAddr
        3 // TODO: different cycle count for new page
      } else {
        2
      }
    },

    Instruction[Relative :: HNil](0x90, "BCC") { case (Relative(relativeAddr) :: HNil, cpu) =>
      if (!cpu.carryFlag) {
        cpu.programCounter += relativeAddr
        3 // TODO: different cycle count for new page
      } else {
        2
      }
    },

    Instruction[Relative :: HNil](0xB0, "BCS") { case (Relative(relativeAddr) :: HNil, cpu) =>
      if (cpu.carryFlag) {
        cpu.programCounter += relativeAddr
        3 // TODO: different cycle count for new page
      } else {
        2
      }
    },

    Instruction[Relative :: HNil](0xF0, "BEQ") { case (Relative(relativeAddr) :: HNil, cpu) =>
      if (cpu.zeroFlag) {
        cpu.programCounter += relativeAddr
        3 // TODO: different cycle count for new page
      } else {
        2
      }
    },

    Instruction[Relative :: HNil](0xD0, "BNE") { case (Relative(relativeAddr) :: HNil, cpu) =>
      if (!cpu.zeroFlag) {
        cpu.programCounter += relativeAddr
        3 // TODO: different cycle count for new page
      } else {
        2
      }
    },

    Instruction[Relative :: HNil](0x10, "BPL") { case (Relative(relativeAddr) :: HNil, cpu) =>
      if (!cpu.negativeFlag) {
        cpu.programCounter += relativeAddr
        3 // TODO: different cycle count for new page
      } else {
        2
      }
    },

    Instruction[Relative :: HNil](0x50, "BVC") { case (Relative(relativeAddr) :: HNil, cpu) =>
      if (!cpu.overflowFlag) {
        cpu.programCounter += relativeAddr
        3 // TODO: different cycle count for new page
      } else {
        2
      }
    },

    Instruction[Relative :: HNil](0x70, "BVS") { case (Relative(relativeAddr) :: HNil, cpu) =>
      if (cpu.overflowFlag) {
        cpu.programCounter += relativeAddr
        3 // TODO: different cycle count for new page
      } else {
        2
      }
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

    Instruction[Immediate :: HNil](0xC9, "CMP") { case (Immediate(const) :: HNil, cpu) =>
      cpu.carryFlag = cpu.accumulator >= const
      cpu.zeroFlag = cpu.accumulator == const
      cpu.negativeFlag = cpu.accumulator < const

      2
    },

    Instruction[Immediate :: HNil](0xE0, "CPX") { case (Immediate(const) :: HNil, cpu) =>
      cpu.carryFlag = cpu.xRegister >= const
      cpu.zeroFlag = cpu.xRegister == const
      cpu.negativeFlag = cpu.xRegister < const

      2
    },

    Instruction[Immediate :: HNil](0xC0, "CPY") { case (Immediate(const) :: HNil, cpu) =>
      cpu.carryFlag = cpu.yRegister >= const
      cpu.zeroFlag = cpu.yRegister == const
      cpu.negativeFlag = cpu.yRegister < const

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

    Instruction[Immediate :: HNil](0x49, "EOR") { case (Immediate(const) :: HNil, cpu) =>
      cpu.accumulator = (toUnsignedInt(cpu.accumulator) ^ toUnsignedInt(const)).toByte
      setZeroNeg(cpu.accumulator, cpu)

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

    Instruction[Immediate :: HNil](0xA9, "LDA") { case (Immediate(const) :: HNil, cpu) =>
      cpu.accumulator = const
      setZeroNeg(cpu.accumulator, cpu)
      2
    },

    Instruction[ZeroPage :: HNil](0xA5, "LDA") { case (addr :: HNil, cpu) =>
      cpu.accumulator = cpu.memory.read(addr.address)
      setZeroNeg(cpu.accumulator, cpu)
      2
    },

    Instruction[Absolute :: HNil](0xAD, "LDA") { case (Absolute(addr) :: HNil, cpu) =>
      cpu.accumulator = cpu.memory.read(addr)
      setZeroNeg(cpu.accumulator, cpu)
      2
    },

    Instruction[Immediate :: HNil](0xA2, "LDX") { case (Immediate(const) :: HNil, cpu) =>
      cpu.xRegister = const
      setZeroNeg(cpu.xRegister, cpu)
      2
    },

    Instruction[Absolute :: HNil](0xAE, "LDX") { case (Absolute(addr) :: HNil, cpu) =>
      cpu.xRegister = cpu.memory.read(addr)
      setZeroNeg(cpu.xRegister, cpu)
      2
    },

    Instruction[Immediate :: HNil](0xA0, "LDY") { case (Immediate(const) :: HNil, cpu) =>
      cpu.yRegister = const
      setZeroNeg(cpu.yRegister, cpu)
      2
    },

    Instruction[HNil](0x4A, "LSR") { (_, cpu) =>
      cpu.carryFlag = (cpu.accumulator & 1) == 1
      cpu.accumulator = (cpu.accumulator >>> 1).toByte
      setZeroNeg(cpu.accumulator, cpu)
      2
    },

    Instruction[HNil](0xEA, "NOP") { (_, _) =>
      2
    },

    Instruction[Immediate :: HNil](0x09, "ORA") { case (Immediate(const) :: HNil, cpu) =>
      cpu.accumulator = (toUnsignedInt(cpu.accumulator) | toUnsignedInt(const)).toByte
      setZeroNeg(cpu.accumulator, cpu)

      2
    },

    Instruction[HNil](0x48, "PHA") { (_, cpu) =>
      pushToStack(cpu.accumulator, cpu)
      3
    },

    Instruction[HNil](0x08, "PHP") { (_, cpu) =>
      def atIndex(value: Boolean, index: Int): Byte = {
        ((if (value) 1 else 0) << index).toByte
      }

      pushToStack(
        (0
          | atIndex(cpu.carryFlag, 0)
          | atIndex(cpu.zeroFlag, 1)
          | atIndex(cpu.interruptDisable, 2)
          | atIndex(cpu.decimalMode, 3)
          | atIndex(true, 4)
          | atIndex(true, 5)
          | atIndex(cpu.overflowFlag, 6)
          | atIndex(cpu.negativeFlag, 7)).toByte,
        cpu
      )

      3
    },

    Instruction[HNil](0x28, "PLP") { (_, cpu) =>
      val stackValue = popFromStack(cpu)

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

    Instruction[HNil](0x2A, "ROL") { (_, cpu) =>
      val newCarry = (cpu.accumulator >> 7) == 1
      cpu.accumulator = ((cpu.accumulator << 1) | (if (cpu.carryFlag) 1 else 0)).toByte
      cpu.carryFlag = newCarry
      2
    },

    Instruction[HNil](0x6A, "ROR") { (_, cpu) =>
      val newCarry = (cpu.accumulator & 1) == 1
      cpu.accumulator = ((cpu.accumulator >>> 1) | ((if (cpu.carryFlag) 1 else 0) << 7)).toByte
      cpu.carryFlag = newCarry
      2
    },

    Instruction[HNil](0x40, "RTI") { (_, cpu) =>
      popFromStack(cpu) // pop cpu status
      val lowerByte = popFromStack(cpu)
      val upperByte = popFromStack(cpu)

      cpu.programCounter = java.lang.Byte.toUnsignedInt(lowerByte) | (java.lang.Byte.toUnsignedInt(upperByte) << 8)

      6
    },

    Instruction[Immediate :: HNil](0xE9, "SBC") { case (Immediate(const) :: HNil, cpu) =>
      val origAcc = toUnsignedInt(cpu.accumulator)
      val subtractedValue = toUnsignedInt(const)
      val sub = origAcc - subtractedValue - (if (cpu.carryFlag) 0 else 1)

      cpu.overflowFlag = ((origAcc ^ sub) & 0x80) != 0 && ((origAcc ^ subtractedValue) & 0x80) != 0

      cpu.accumulator = sub.toByte
      setZeroNeg(cpu.accumulator, cpu)
      cpu.carryFlag = sub >= 0

      2
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
    },

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
    Instruction[IndirectIndexed :: HNil](0x91, "STA") { case (ind :: HNil, cpu) =>
      println(ind.address.formatted("0x%x"))
      cpu.memory.write(ind.address, cpu.accumulator)
      6
    },

    Instruction[ZeroPage :: HNil](0x86, "STX") { case (ZeroPage(address) :: HNil, cpu) =>
      cpu.memory.write(address, cpu.xRegister)
      3
    },
    Instruction[Absolute :: HNil](0x8E, "STX") { case (Absolute(address) :: HNil, cpu) =>
      cpu.memory.write(address, cpu.xRegister)
      4
    },

    Instruction[ZeroPage :: HNil](0x84, "STY") { case (ZeroPage(address) :: HNil, cpu) =>
      cpu.memory.write(address, cpu.xRegister)
      3
    },

    Instruction[HNil](0xAA, "TAX") { (_, cpu) =>
      cpu.xRegister = cpu.accumulator
      setZeroNeg(cpu.xRegister, cpu)

      2
    },

    Instruction[HNil](0xA8, "TAY") { (_, cpu) =>
      cpu.yRegister = cpu.accumulator
      setZeroNeg(cpu.yRegister, cpu)

      2
    },

    Instruction[HNil](0xBA, "TSX") { (_, cpu) =>
      cpu.xRegister = cpu.stackPointer

      cpu.zeroFlag = cpu.xRegister == 0
      cpu.negativeFlag = cpu.xRegister < 0

      2
    },

    Instruction[HNil](0x8A, "TXA") { (_, cpu) =>
      cpu.accumulator = cpu.xRegister
      setZeroNeg(cpu.accumulator, cpu)

      2
    },

    Instruction[HNil](0x9A, "TXS") { (_, cpu) =>
      cpu.stackPointer = cpu.xRegister
      2
    },

    Instruction[HNil](0x98, "TYA") { (_, cpu) =>
      cpu.accumulator = cpu.yRegister
      setZeroNeg(cpu.accumulator, cpu)

      2
    }
  ).map(i => i.opcode.toByte -> i).toMap
}
