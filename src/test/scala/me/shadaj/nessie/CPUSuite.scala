package me.shadaj.nessie

import java.io.File

import org.scalatest.FunSuite

import scala.io.Source

class CPUSuite extends FunSuite {
  private val nestest = NESFile.fromFile(new File("test-roms/nestest.nes"))

  test("Can run nestest ROM through invalid opcodes (0xA3)") {
    val memory = new Memory(Seq(
      new NESRam, new PPU(null, null, _ => {}).cpuMemoryMapping,
      new Mapper0(nestest.programRom, nestest.chrRom, nestest.verticalMirror)
    ))

    val cpu = new CPU(memory)

    cpu.programCounter = 0xC000

    val expectedLines = Source.fromFile("test-roms/nestest.log").getLines()

    var stopRunning = false
    var ticks = 0
    while (!stopRunning) {
      val currentLogLine = expectedLines.next()
      val split = currentLogLine.split(' ').filterNot(_.isEmpty)

      assert(cpu.programCounter == Integer.parseInt(split.head, 16))
      assert(java.lang.Byte.toUnsignedInt(cpu.accumulator) == Integer.parseInt(split.find(_.startsWith("A:")).get.drop(2), 16))
      assert(java.lang.Byte.toUnsignedInt(cpu.xRegister) == Integer.parseInt(split.find(_.startsWith("X:")).get.drop(2), 16))
      assert(java.lang.Byte.toUnsignedInt(cpu.yRegister) == Integer.parseInt(split.find(_.startsWith("Y:")).get.drop(2), 16))
      assert(java.lang.Byte.toUnsignedInt(cpu.stackPointer) == Integer.parseInt(split.find(_.startsWith("SP:")).get.drop(3), 16))

      assert((ticks * 3 % 341) == split.last.split(':').last.toInt)

      if (cpu.memory.read(cpu.programCounter) == 0xA3.toByte) {
        stopRunning = true
      } else {
        ticks += cpu.tick
      }
    }
  }
}
