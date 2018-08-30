package me.shadaj.nessie

class Console(file: NESFile, drawFrame: Array[Array[(Int, Int, Int)]] => Unit, currentButtonState: () => Seq[Boolean], extraMemoryProviders: Seq[MemoryProvider] = Seq.empty) {
  val ppu = new PPU(() => {
    cpu.runNMI
  }, new MemoryProvider {
    override def canReadAt(address: Int): Boolean = address < 0x2000
    override def canWriteAt(address: Int): Boolean = false

    override def read(address: Int, memory: Memory): Byte = {
      if (address < file.chrRom.length) {
        file.chrRom(address)
      } else 0
    }

    override def write(address: Int, value: Byte, memory: Memory): Unit = ???
  }, drawFrame)

  val memory = new Memory(Seq(
    new NESRam,
    ppu.cpuMemoryMapping,
    new APUIORegisters,
    new ControllerRegisters(currentButtonState),
    file.mapperNumber match {
      case 0 => new Mapper0(file.programRom)
      case 1 => new Mapper1(file.programRom)
    }
  ) ++ extraMemoryProviders)

  lazy val cpu: CPU = new CPU(memory)

  def tick(): Boolean = {
    val cpuCycles = cpu.tick
    (1 to cpuCycles * 3).exists(_ => ppu.step())
  }
}
