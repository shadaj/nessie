package me.shadaj.nessie

import shapeless._
import shapeless.ops.hlist.ToList

sealed trait Arg

trait ArgParser[A <: Arg] {
  val size: Int
  def parse(getArg: Int => Byte): A
}

sealed trait Address extends Arg

case class Immediate(constant: Byte) extends Address {
  override def toString: String = constant.formatted("#%x".toUpperCase)
}

object Immediate {
  implicit val parser: ArgParser[Immediate] = new ArgParser[Immediate] {
    override val size: Int = 1

    override def parse(getArg: Int => Byte): Immediate = Immediate(getArg(0))
  }
}

case class Absolute(twoBytes: Int) extends Address {
  override def toString: String = twoBytes.formatted("$%x".toUpperCase)
}

object Absolute {
  implicit val parser: ArgParser[Absolute] = new ArgParser[Absolute] {
    override val size: Int = 2

    override def parse(getArg: Int => Byte): Absolute = {
      import java.lang.Byte.toUnsignedInt
      Absolute(
        toUnsignedInt(getArg(0)) | (toUnsignedInt(getArg(1)) << 8)
      )
    }
  }
}

trait ArgsParser[Args <: HList] {
  val size: Int
  def parse(getArg: Int => Byte): Args
}

object ArgsParser {
  implicit val forNil: ArgsParser[HNil] = new ArgsParser[HNil] {
    override val size: Int = 0
    override def parse(getArg: Int => Byte): HNil = HNil
  }

  implicit def forCons[Head <: Arg, Tail <: HList](implicit argParser: ArgParser[Head], tailParser: ArgsParser[Tail]): ArgsParser[Head :: Tail] = new ArgsParser[Head :: Tail] {
    override val size: Int = argParser.size + tailParser.size
    override def parse(getArg: Int => Byte): Head :: Tail = {
      argParser.parse(getArg) :: tailParser.parse(i => getArg(i + argParser.size))
    }
  }
}

case class Instruction[Args <: HList](opcode: Int, name: String)(execute: (Args, CPU) => Int)(implicit parseArgs: ArgsParser[Args], argsToList: ToList[Args, Arg]) {
  val argsSize = parseArgs.size
  def parseArgs(argsBytes: Seq[Byte]): List[Arg] = parseArgs.parse(argsBytes).toList
  def run(getArg: Int => Byte, log: Boolean = false)(cpu: CPU) = {
    val parsedArgs = parseArgs.parse(getArg)
    if (log) {
      println(s"$this ${parsedArgs.toList.mkString(" ")}")
    }
    execute(parsedArgs, cpu)
  }

  override def toString: String = name
}

object Instruction {
  val myArguments: Immediate :: HNil = Immediate(1) :: HNil

  val cpuInstructions: Map[Byte, Instruction[_]] = Seq(
    Instruction[HNil](0xD8, "CLD") { case (_, cpu) =>
      cpu.decimalMode = false
      2
    },

    Instruction[Immediate :: HNil](0xC0, "CPY") { case (Immediate(const) :: HNil, cpu) =>
      if (cpu.yRegister >= const) {
        cpu.carryFlag = true
      }

      if (cpu.yRegister == const) {
        cpu.zeroFlag = true
      }

      if (cpu.yRegister < const) {
        cpu.negativeFlag = true
      }

      2
    },

    Instruction[HNil](0xE8, "INX") { (_, cpu) =>
      cpu.xRegister = (cpu.xRegister + 1).toByte
      cpu.zeroFlag = cpu.xRegister == 0
      cpu.negativeFlag = cpu.xRegister < 0
      2
    },

    Instruction[Absolute :: HNil](0x4C, "JMP") { case (Absolute(address) :: HNil, cpu) =>
      cpu.programCounter = address
      3
    },

    Instruction[Immediate :: HNil](0xA9, "LDA") { case (Immediate(const) :: HNil, cpu) =>
      cpu.accumulator = const
      2
    },

    Instruction[Immediate :: HNil](0xA2, "LDX") { case (Immediate(const) :: HNil, cpu) =>
      cpu.xRegister = const
      2
    },

    Instruction[HNil](0x78, "SEI") { (_, cpu) =>
      cpu.interruptDisable = true
      2
    },

    Instruction[Absolute :: HNil](0x8D, "STA") { case (Absolute(address) :: HNil, cpu) =>
      cpu.accumulator = cpu.memory.read(address)
      4
    },

    Instruction[Absolute :: HNil](0x8E, "STX") { case (Absolute(address) :: HNil, cpu) =>
      cpu.memory.write(address, cpu.xRegister)
      4
    },

    Instruction[HNil](0x9A, "TXS") { (_, cpu) =>
      cpu.stackPointer = cpu.xRegister
      2
    }
  ).map(i => i.opcode.toByte -> i).toMap
}
