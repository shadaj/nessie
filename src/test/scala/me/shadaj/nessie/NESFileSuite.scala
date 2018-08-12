package me.shadaj.nessie

import java.io.File

import org.scalatest.FunSuite

class NESFileSuite extends FunSuite {
  test("Can load .nes files") {
    val loaded = NESFile.fromFile(new File("test-roms/01-basics.nes"))
    assert(loaded.programRom.length == 32 * 1024)
    assert((loaded.mapperNumber & 0xFF) == 0)
  }
}
