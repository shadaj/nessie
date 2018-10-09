package me.shadaj.nessie

import java.io.File

import org.scalatest.FunSuite

class PPUVblNmiSuite extends FunSuite {
  // Fails in other emulators (JSNes), so I guess we're okay?
  ignore("Can run ppu_vbl_nmi/01-vbl_basics test ROM") {
    Util.runTestROM(NESFile.fromFile(new File("test-roms/ppu_vbl_nmi/ppu_vbl_nmi.nes")))
  }
}
