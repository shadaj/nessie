package me.shadaj.nessie

import java.io.File

import org.scalatest.FunSuite

import scala.io.Source

class CPUSuite extends FunSuite {
  private val nestest = NESFile.fromFile(new File("test-roms/nestest.nes"))
  private val basics = NESFile.fromFile(new File("test-roms/01-basics.nes"))
  private val officialOnly = NESFile.fromFile(new File("test-roms/official_only.nes"))

  test("Can execute some instructions from basic test ROM and report cycle count") {
    val memory = new Memory(Seq(new NESRam, new PPURegisters, new Mapper0(basics.programRom)))
    val cpu = new CPU(memory)
    assert(cpu.tick == 2) // SEI
    assert(cpu.tick == 3) // JMP $EB12
    assert(cpu.tick == 4) // STA $224
    assert(cpu.tick == 2) // LDA #0
    assert(cpu.tick == 4) // STA $2000
    assert(cpu.tick == 4) // STA $2001
  }

  test("Can run nestest ROM through invalid opcodes (0x04)") {
    val memory = new Memory(Seq(new NESRam, new PPURegisters, new Mapper0(nestest.programRom)))
    val cpu = new CPU(memory)

    cpu.programCounter = 0xC000

    val expectedLines = Source.fromFile("test-roms/nestest.log").getLines()

    var stopRunning = false
    while (!stopRunning) {
      val currentLogLine = expectedLines.next()
      val split = currentLogLine.split(' ').filterNot(_.isEmpty)
      if (cpu.memory.read(cpu.programCounter) == 0x04) {
        stopRunning = true
      } else {
        assert(cpu.programCounter == Integer.parseInt(split.head, 16))
        assert(java.lang.Byte.toUnsignedInt(cpu.accumulator) == Integer.parseInt(split.find(_.startsWith("A:")).get.drop(2), 16))
        assert(java.lang.Byte.toUnsignedInt(cpu.xRegister) == Integer.parseInt(split.find(_.startsWith("X:")).get.drop(2), 16))
        assert(java.lang.Byte.toUnsignedInt(cpu.yRegister) == Integer.parseInt(split.find(_.startsWith("Y:")).get.drop(2), 16))
        assert(java.lang.Byte.toUnsignedInt(cpu.stackPointer) == Integer.parseInt(split.find(_.startsWith("SP:")).get.drop(3), 16))
        cpu.tick
      }
    }
  }

  def runTestROM(file: NESFile) = {
    var isDone = false

    val memory = new Memory(Seq(new NESRam, new PPURegisters, new APUIORegisters,
      if (file.mapperNumber == 0) new Mapper0(file.programRom) else new Mapper1(file.programRom),
      new MemoryProvider {
      private val stringMemory = new Array[Byte](256)
      override def contains(address: Int): Boolean = address >= 0x6000

      override def read(address: Int): Byte = ???

      override def write(address: Int, value: Byte): Unit = {
        if (address >= 0x6004) {
          stringMemory(address - 0x6004) = value
        }

        if (address == 0x6000 && value == 0) {
          println(Iterator.from(0).map(stringMemory.apply).takeWhile(_ != 0).map(_.toChar).mkString)
          isDone = true
        }
      }
    }))

    val cpu = new CPU(memory)

    while (!isDone) {
      cpu.tick
    }
  }

  test("Can run 01-basics test ROM") {
    runTestROM(NESFile.fromFile(new File("test-roms/01-basics.nes")))
  }

  test("Can run 02-implied test ROM") {
    runTestROM(NESFile.fromFile(new File("test-roms/02-implied.nes")))
  }

  test("Can run official_only test ROM") {
    runTestROM(NESFile.fromFile(new File("test-roms/official_only.nes")))
  }
}
