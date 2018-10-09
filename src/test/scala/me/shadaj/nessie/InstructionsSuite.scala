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

  ignore("Can run 03-immediate test ROM") {
    Util.runTestROM(NESFile.fromFile(new File("test-roms/instr_test-v5/rom_singles/03-immediate.nes")))
  }

  ignore("Can run 04-zero_page test ROM") {
    Util.runTestROM(NESFile.fromFile(new File("test-roms/instr_test-v5/rom_singles/04-zero_page.nes")))
  }

  ignore("Can run 05-zp_xy test ROM") {
    Util.runTestROM(NESFile.fromFile(new File("test-roms/instr_test-v5/rom_singles/05-zp_xy.nes")))
  }

  ignore("Can run 06-absolute test ROM") {
    Util.runTestROM(NESFile.fromFile(new File("test-roms/instr_test-v5/rom_singles/06-absolute.nes")))
  }

  ignore("Can run 07-abs_xy test ROM") {
    Util.runTestROM(NESFile.fromFile(new File("test-roms/instr_test-v5/rom_singles/07-abs_xy.nes")))
  }

  ignore("Can run 08-ind_x test ROM") {
    Util.runTestROM(NESFile.fromFile(new File("test-roms/instr_test-v5/rom_singles/08-ind_x.nes")))
  }

  ignore("Can run 09-ind_y test ROM") {
    Util.runTestROM(NESFile.fromFile(new File("test-roms/instr_test-v5/rom_singles/09-ind_y.nes")))
  }

  test("Can run 10-branches test ROM") {
    Util.runTestROM(NESFile.fromFile(new File("test-roms/instr_test-v5/rom_singles/10-branches.nes")))
  }

  test("Can run 11-stack test ROM") {
    Util.runTestROM(NESFile.fromFile(new File("test-roms/instr_test-v5/rom_singles/11-stack.nes")))
  }

  test("Can run 12-jmp_jsr test ROM") {
    Util.runTestROM(NESFile.fromFile(new File("test-roms/instr_test-v5/rom_singles/12-jmp_jsr.nes")))
  }

  test("Can run 13-rts test ROM") {
    Util.runTestROM(NESFile.fromFile(new File("test-roms/instr_test-v5/rom_singles/13-rts.nes")))
  }

  test("Can run 14-rti test ROM") {
    Util.runTestROM(NESFile.fromFile(new File("test-roms/instr_test-v5/rom_singles/14-rti.nes")))
  }

  test("Can run 15-brk test ROM") {
    Util.runTestROM(NESFile.fromFile(new File("test-roms/instr_test-v5/rom_singles/15-brk.nes")))
  }

  test("Can run 16-special test ROM") {
    Util.runTestROM(NESFile.fromFile(new File("test-roms/instr_test-v5/rom_singles/16-special.nes")))
  }

  test("Can run official_only test ROM") {
    Util.runTestROM(NESFile.fromFile(new File("test-roms/instr_test-v5/official_only.nes")))
  }
}
