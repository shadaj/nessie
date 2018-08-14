package me.shadaj.nessie

import java.io.File

import org.scalatest.FunSuite

import scala.io.Source

class CPUSuite extends FunSuite {
  private val nestest = NESFile.fromFile(new File("test-roms/nestest.nes"))
  private val basics = NESFile.fromFile(new File("test-roms/01-basics.nes"))

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

  test("Can run full basic test ROM") {
    val memory = new Memory(Seq(new NESRam, new PPURegisters, new Mapper0(nestest.programRom)))
    val cpu = new CPU(memory)

    cpu.programCounter = 0xC000

    val expectedLines = Source.fromFile("test-roms/nestest.log").getLines()

    println("------ begin basic test ------")
    while (true) {
      val currentLogLine = expectedLines.next()
      val split = currentLogLine.split(' ').filterNot(_.isEmpty)
      assert(cpu.programCounter == Integer.parseInt(split.head, 16))
      assert(java.lang.Byte.toUnsignedInt(cpu.accumulator) == Integer.parseInt(split.find(_.startsWith("A:")).get.drop(2), 16))
      assert(java.lang.Byte.toUnsignedInt(cpu.xRegister) == Integer.parseInt(split.find(_.startsWith("X:")).get.drop(2), 16))
      assert(java.lang.Byte.toUnsignedInt(cpu.yRegister) == Integer.parseInt(split.find(_.startsWith("Y:")).get.drop(2), 16))
      assert(java.lang.Byte.toUnsignedInt(cpu.stackPointer) == Integer.parseInt(split.find(_.startsWith("SP:")).get.drop(3), 16))
      cpu.tick(true)
    }
  }
}
