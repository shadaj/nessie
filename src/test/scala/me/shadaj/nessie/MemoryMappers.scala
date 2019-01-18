package me.shadaj.nessie

import java.io.File

import org.scalatest.FunSuite

class MemoryMappers extends FunSuite {
  private val basics = NESFile.fromFile(new File("test-roms/instr_test-v5/rom_singles/01-basics.nes"))

  test("Mapper 0 can load entrypoint vector from basic ROM") {
    val memory = new Memory(Seq(new Mapper0(basics.programRom, basics.chrRom, basics.verticalMirror)))
    assert(memory.readTwoBytes(0xFFFC) == 0xE683)
  }

  test("Mapper 0 can read entrypoint instruction from basic ROM") {
    val memory = new Memory(Seq(new Mapper0(basics.programRom, basics.chrRom, basics.verticalMirror)))
    assert(memory.read(0xE683) == 0x78)
  }
}
