package me.shadaj.nessie

import java.io.File

import org.scalatest.FunSuite

class InstructionsSuite extends FunSuite {
  test("Can run 01-basics test ROM") {
    Util.runTestROM(NESFile.fromFile(new File("test-roms/instr_test-v5/rom_singles/01-basics.nes")))
  }

  test("Can run 02-implied test ROM") {
    Util.runTestROM(NESFile.fromFile(new File("test-roms/instr_test-v5/rom_singles/02-implied.nes")))
  }

  test("Can run 15-brk test ROM") {
    Util.runTestROM(NESFile.fromFile(new File("test-roms/instr_test-v5/rom_singles/15-brk.nes")))
  }

  test("Can run official_only test ROM") {
    Util.runTestROM(NESFile.fromFile(new File("test-roms/instr_test-v5/official_only.nes")))
  }
}
