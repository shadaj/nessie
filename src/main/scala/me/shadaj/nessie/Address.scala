package me.shadaj.nessie

import java.lang.Byte.toUnsignedInt

trait Address extends Arg

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
  override def toString: String = f"${"$"}$indirect%x,$x (zeropage,x)"
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

case class ZeroPageY(indirect: Int, y: Int) extends Address {
  val address = (indirect + y) & 0xFF // wraps around
  override def toString: String = f"${"$"}$indirect%x,$y (zeropage,y)"
}

object ZeroPageY {
  implicit val parser: ArgParser[ZeroPageY] = new ArgParser[ZeroPageY] {
    override val size: Int = 1

    override def parse(getArg: Int => Byte, cpu: CPU): ZeroPageY = {
      ZeroPageY(
        java.lang.Byte.toUnsignedInt(getArg(0)),
        toUnsignedInt(cpu.yRegister)
      )
    }
  }
}

case class Absolute(twoBytes: Int) extends Address {
  val address = twoBytes
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

case class AbsoluteY(absolute: Int, y: Int) extends Address {
  val address = (absolute + y) & 0xFFFF
  override def toString: String = f"${"$"}$absolute%x,Y=$y = $address%X"
}

object AbsoluteY {
  implicit val parser: ArgParser[AbsoluteY] = new ArgParser[AbsoluteY] {
    override val size: Int = 2

    override def parse(getArg: Int => Byte, cpu: CPU): AbsoluteY = {
      import java.lang.Byte.toUnsignedInt
      AbsoluteY(
        toUnsignedInt(getArg(0)) | (toUnsignedInt(getArg(1)) << 8),
        toUnsignedInt(cpu.yRegister)
      )
    }
  }
}

case class Indirect(indirect: Int, address: Int) extends Address {
  override def toString: String = f"(${"$"}$indirect%X) = $address%X"
}

object Indirect {
  implicit val parser: ArgParser[Indirect] = new ArgParser[Indirect] {
    override val size: Int = 2

    override def parse(getArg: Int => Byte, cpu: CPU): Indirect = {
      import java.lang.Byte.toUnsignedInt
      val indirect = toUnsignedInt(getArg(0)) | (toUnsignedInt(getArg(1)) << 8)
      Indirect(indirect, cpu.memory.readTwoBytesBug(indirect))
    }
  }
}

case class IndirectIndexed(indirect: Int, y: Int) extends Address {
  val address = (indirect + y) & 0xFFFF
  override def toString: String = f"${"$"}$indirect%x,$y (ind,y)"
}

object IndirectIndexed {
  implicit val parser: ArgParser[IndirectIndexed] = new ArgParser[IndirectIndexed] {
    override val size: Int = 1

    override def parse(getArg: Int => Byte, cpu: CPU): IndirectIndexed = {
      val zeroPageAddress = cpu.memory.readTwoBytesBug(java.lang.Byte.toUnsignedInt(getArg(0)))

      IndirectIndexed(zeroPageAddress, toUnsignedInt(cpu.yRegister))
    }
  }
}

case class IndirectX(indirect: Int, x: Int, resolvedAddress: Int) extends Address {
  val address = resolvedAddress
  override def toString: String = f"(${"$"}$indirect%X,X=$x) @ ${(indirect + x) & 0xFF}%X = $resolvedAddress%X"
}

object IndirectX {
  implicit val parser: ArgParser[IndirectX] = new ArgParser[IndirectX] {
    override val size: Int = 1

    override def parse(getArg: Int => Byte, cpu: CPU): IndirectX = {
      val indirect = java.lang.Byte.toUnsignedInt(getArg(0))
      val zeroPageAddress = cpu.memory.readTwoBytesBug(
        (indirect + toUnsignedInt(cpu.xRegister)) & 0xFF
      )

      IndirectX(indirect, toUnsignedInt(cpu.xRegister), zeroPageAddress)
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

case object Accumulator extends Address
