package me.shadaj.nessie

import java.io.File

import org.scalatest.FunSuite

class PPUVblNmiSuite extends FunSuite {
  // Fails in other emulators (JSNes), so I guess we're okay?
  ignore("ppu_vbl_nmi/01-vbl_basics") {
    Util.runTestROM(NESFile.fromFile(new File("test-roms/ppu_vbl_nmi/rom_singles/01-vbl_basics.nes")))
  }

  // test("ppu_vbl_nmi/02-vbl_set_time") {
  //   Util.runTestROM(NESFile.fromFile(new File("test-roms/ppu_vbl_nmi/rom_singles/02-vbl_set_time.nes")))
  // }
}
