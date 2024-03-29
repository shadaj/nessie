package me.shadaj.nessie

import java.io.File

import org.scalatest.FunSuite

import scala.io.Source

class NESFileSuite extends FunSuite {
  test("Can load .nes files") {
    val loaded = NESFile.fromFile(new File("test-roms/instr_test-v5/rom_singles/01-basics.nes"))
    assert(loaded.programRom.length == 32 * 1024)
    assert((loaded.mapperNumber & 0xFF) == 0)
  }
}
