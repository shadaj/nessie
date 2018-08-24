package me.shadaj.nessie

class Console(file: NESFile, drawFrame: Array[Array[(Int, Int, Int)]] => Unit) {
  val ppu = new PPU(() => {
    cpu.runNMI
  }, new MemoryProvider {
    override def contains(address: Int): Boolean = address < 0x2000

    override def read(address: Int, memory: Memory): Byte = file.chrRom(address)

    override def write(address: Int, value: Byte, memory: Memory): Unit = ???
  }, drawFrame)

  println(file.mapperNumber)

  val memory = new Memory(Seq(new NESRam, ppu.cpuMemoryMapping, new APUIORegisters, if (file.mapperNumber == 0) new Mapper0(file.programRom) else new Mapper1(file.programRom)))
  lazy val cpu: CPU = new CPU(memory)

  def tick(): Unit = {
    val cpuCycles = cpu.tick
    (1 to cpuCycles * 3).foreach(_ => ppu.step())
  }
}
