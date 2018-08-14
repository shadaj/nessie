package me.shadaj.nessie

trait MemoryProvider {
  def contains(address: Int): Boolean
  def read(address: Int): Byte
  def write(address: Int, value: Byte): Unit
}

class Memory(providers: Seq[MemoryProvider]) {
  def read(address: Int): Byte = providers.find(_.contains(address)).get.read(address)

  def write(address: Int, value: Byte): Unit = {
    providers.find(_.contains(address)).get.write(address, value)
  }

  def readTwoBytes(address: Int): Int = {
    import java.lang.Byte.toUnsignedInt
    toUnsignedInt(read(address)) | (toUnsignedInt(read(address + 1)) << 8)
  }

  // 6502 has a bug in indirect mode where the low byte wraps around without the upper byte changing
  def readTwoBytesBug(address: Int): Int = {
    import java.lang.Byte.toUnsignedInt
    val highAddress = (address & 0xFF00) | ((address + 1) & 0xFF)
    toUnsignedInt(read(address)) | (toUnsignedInt(read(highAddress)) << 8)
  }
}

class NESRam extends MemoryProvider {
  val memory = new Array[Byte](2048)

  override def contains(address: Int): Boolean = address >= 0x0 && address <= 0x1FFF

  override def read(address: Int): Byte = {
    memory(address % 2048)
  }

  def write(address: Int, value: Byte): Unit = {
    memory(address % 2048) = value
  }
}

class Mapper0(prgRom: Array[Byte]) extends MemoryProvider {
  override def contains(address: Int): Boolean = address >= 0x8000
  override def read(address: Int): Byte = {
    if (address >= 0x8000 && address < 0xC000) {
      prgRom(address - 0x8000)
    } else if (address >= 0xC000) {
      if (prgRom.length > 16 * 1024) {
        prgRom(address - 0x8000)
      } else {
        // mirrored memory
        prgRom(address - 0xC000)
      }
    } else {
      throw new IllegalArgumentException(s"Cannot read program ROM at address $address")
    }
  }

  override def write(address: Int, value: Byte): Unit = {
    throw new UnsupportedOperationException(s"Cannot write to program ROM, address $address was given")
  }
}

class PPURegisters extends MemoryProvider {
  val memory = new Array[Byte](8)

  override def contains(address: Int): Boolean = address >= 0x2000 && address < 0x4000

  override def read(address: Int): Byte = {
    memory((address - 0x2000) % 8 /* mirroring! */)
  }

  override def write(address: Int, value: Byte): Unit = {
    // TODO: some ppu registers are read-only
    memory((address - 0x2000) % 8 /* mirroring! */) = value
  }
}

class APUIORegisters extends MemoryProvider {
  val memory = new Array[Byte](24)

  override def contains(address: Int): Boolean = address >= 0x4000 && address < 0x4018

  override def read(address: Int): Byte = {
    memory(address - 0x4000)
  }

  override def write(address: Int, value: Byte): Unit = {
    // TODO: are registers read only?
    memory(address - 0x4000) = value
  }
}