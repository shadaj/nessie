package me.shadaj.nessie

trait MemoryProvider {
  def canReadAt(address: Int): Boolean
  def canWriteAt(address: Int): Boolean

  def read(address: Int, memory: Memory): Byte
  def write(address: Int, value: Byte, memory: Memory): Unit
}

trait PPUMemoryProvider {
  val ppuMemory: MemoryProvider
}

class Memory(providersGet: Seq[MemoryProvider]) {
  private lazy val providers = providersGet

  def read(address: Int): Byte = providers.find(_.canReadAt(address)).map(_.read(address, this)).getOrElse {
    println(f"WARN: cannot write to address 0x$address%X, no-op")
    0.toByte
  }

  def write(address: Int, value: Byte): Unit = {
    providers.find(_.canWriteAt(address)) match {
      case Some(provider) =>
        provider.write(address, value, this)
      case None =>
        println(f"WARN: cannot write to address 0x$address%X, no-op")
    }
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

  override def canReadAt(address: Int): Boolean = address >= 0x0 && address <= 0x1FFF
  override def canWriteAt(address: Int): Boolean = address >= 0x0 && address <= 0x1FFF

  override def read(address: Int, memoryAccess: Memory): Byte = {
    memory(address % 2048)
  }

  def write(address: Int, value: Byte, memoryAccess: Memory): Unit = {
    memory(address % 2048) = value
  }
}

class Mapper0(prgRom: Array[Byte], chrRom: Array[Byte], verticalMirror: Boolean) extends MemoryProvider with PPUMemoryProvider  {
  override def canReadAt(address: Int): Boolean = address >= 0x4020
  override def canWriteAt(address: Int): Boolean = false

  override def read(address: Int, memory: Memory): Byte = {
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
      0xFF.toByte
    }
  }

  override def write(address: Int, value: Byte, memory: Memory): Unit = {
    throw new UnsupportedOperationException(s"Cannot write to program ROM, address $address was given")
  }

  override val ppuMemory: MemoryProvider = new MemoryProvider {
    private val localChrRom = if (chrRom.length == 0) new Array[Byte](0x2000) else chrRom
    private val nametableMemory = new Array[Byte](2048)

    override def canReadAt(address: Int): Boolean = address < 0x3F00
    override def canWriteAt(address: Int): Boolean = address < 0x3F00

    override def read(address: Int, memory: Memory): Byte = {
      if (address < 0x2000) {
        if (address < localChrRom.length) {
          localChrRom(address)
        } else 0
      } else if (address >= 0x2000 && address < 0x3F00) {
        nametableMemory({
          if (!verticalMirror) {
            (if (address >= 0x2800) 0x400 else 0) + (address % 0x400)
          } else {
            (address - 0x2000) % 0x800
          }
        })
      } else ???
    }

    override def write(address: Int, value: Byte, memory: Memory): Unit = {
      if (address < 0x2000) {
        // allow writes to support Blargg PPU tests
        localChrRom(address) = value
      } else if (address >= 0x2000 && address < 0x3F00) {
        nametableMemory({
          if (!verticalMirror) {
            (if (address >= 0x2800) 0x400 else 0) + (address % 0x400)
          } else {
            (address - 0x2000) % 0x800 // vertical mirroring
          }
        }) = value
      }
    }
  }
}

class Mapper1(prgRom: Array[Byte], chrRom: Array[Byte]) extends MemoryProvider with PPUMemoryProvider {
  override def canReadAt(address: Int): Boolean = address >= 0x8000
  override def canWriteAt(address: Int): Boolean = address >= 0x8000

  override def read(address: Int, memory: Memory): Byte = {
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

  override def write(address: Int, value: Byte, memory: Memory): Unit = {
    // TODO: figure out when writing is actually allowed
  }

  override val ppuMemory: MemoryProvider = new MemoryProvider {
    private val nametableMemory = new Array[Byte](2048)

    override def canReadAt(address: Int): Boolean = address < 0x3F00
    override def canWriteAt(address: Int): Boolean = address < 0x3F00

    override def read(address: Int, memory: Memory): Byte = {
      if (address < 0x2000) {
        if (address < chrRom.length) {
          chrRom(address)
        } else 0
      } else if (address >= 0x2000 && address < 0x3F00) {
        nametableMemory((address - 0x2000) % 0x800) // vertical mirroring
      } else ???
    }

    override def write(address: Int, value: Byte, memory: Memory): Unit = {
      if (address < 0x2000) {} else if (address >= 0x2000 && address < 0x3F00) {
        nametableMemory((address - 0x2000) % 0x800) = value // vertical mirroring
      }
    }
  }
}

class Mapper2(prgRom: Array[Byte]) extends MemoryProvider with PPUMemoryProvider {
  override def canReadAt(address: Int): Boolean = address >= 0x8000
  override def canWriteAt(address: Int): Boolean = address >= 0x8000

  private val banks = prgRom.grouped(16 * 1024).toArray // 16 KB banks

  private var bankSelect: Byte = 0

  override def read(address: Int, memory: Memory): Byte = {
    if (address >= 0xC000) {
      banks(banks.length - 1)(address - 0xC000)
    } else if (address >= 0x8000) {
      banks(bankSelect)(address - 0x8000)
    } else {
      throw new IllegalArgumentException(s"Cannot read program ROM at address $address")
    }
  }

  override def write(address: Int, value: Byte, memory: Memory): Unit = {
    if (address >= 0x8000) {
      bankSelect = value
    } else ???
    // TODO: figure out when writing is actually allowed
  }

  override val ppuMemory: MemoryProvider = new MemoryProvider {
    private val otherMemory = new Array[Byte](8 * 1024)
    private val nametableMemory = new Array[Byte](2048)

    override def canReadAt(address: Int): Boolean = address < 0x3F00
    override def canWriteAt(address: Int): Boolean = address < 0x3F00

    override def read(address: Int, memory: Memory): Byte = {
      if (address < 0x2000) {
        otherMemory(address)
      } else if (address >= 0x2000 && address < 0x3F00) {
        nametableMemory((address - 0x2000) % 0x800) // vertical mirroring
      } else ???
    }

    override def write(address: Int, value: Byte, memory: Memory): Unit = {
      if (address < 0x2000) {
        otherMemory(address) = value
      } else if (address >= 0x2000 && address < 0x3F00) {
        nametableMemory((address - 0x2000) % 0x800) = value // vertical mirroring
      }
    }
  }
}

class ControllerRegisters(currentButtonState: () => Seq[Boolean]) extends MemoryProvider {
  private var incrementIndex = false
  private var currentIndex = 0

  override def canReadAt(address: Int): Boolean = address == 0x4016 || address == 0x4017
  override def canWriteAt(address: Int): Boolean = address == 0x4016

  override def read(address: Int, memory: Memory): Byte = {
    val state = currentButtonState()
    address & 0xFF match {
      case 0x16 =>
        if (currentIndex >= 8) 1 else {
          val ret: Byte = if (state(currentIndex)) {
            1
          } else 0

          if (incrementIndex) {
            currentIndex += 1
          }

          ret
        }
      case 0x17 => 0
    }
  }

  override def write(address: Int, value: Byte, memory: Memory): Unit = {
    address match {
      case 0x4016 =>
        if ((value & 1) == 1) {
          currentIndex = 0
          incrementIndex = false
        } else {
          incrementIndex = true
        }
    }
  }
}