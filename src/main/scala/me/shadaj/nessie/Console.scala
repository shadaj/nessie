package me.shadaj.nessie

class Console(file: NESFile, drawFrame: Array[Array[(Int, Int, Int)]] => Unit, currentButtonState: () => Seq[Boolean], extraMemoryProviders: Seq[MemoryProvider] = Seq.empty) {
  println(s"Mapper: ${file.mapperNumber}")
  private val memoryProviders = Seq(
    new NESRam,
    new APU,
    new ControllerRegisters(currentButtonState),
    file.mapperNumber match {
      case 0 => new Mapper0(file.programRom, file.chrRom, file.verticalMirror)
      case 1 => new Mapper1(file.programRom, file.chrRom)
      case 2 => new Mapper2(file.programRom)
    }
  ) ++ extraMemoryProviders

  val ppu = new PPU(() => {
    cpu.runNMI
  }, new Memory(memoryProviders.collect {
    case m: PPUMemoryProvider => m.ppuMemory
  }), drawFrame)

  lazy val cpu: CPU = new CPU(new Memory(
    memoryProviders :+ ppu.cpuMemoryMapping
  ))

  def tick(log: Boolean = false): Boolean = {
    val cpuCycles = cpu.tick(log)
    (1 to cpuCycles * 3).exists(_ => ppu.step())
  }
}
