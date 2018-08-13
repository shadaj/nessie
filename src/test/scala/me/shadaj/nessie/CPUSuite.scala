package me.shadaj.nessie

import java.io.File

import org.scalatest.FunSuite

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

    println("------ begin basic test ------")
    while (true) {
      cpu.tick(true)
    }
  }
}
