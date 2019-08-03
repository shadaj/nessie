package me.shadaj.nessie

import java.io.File

import org.scalatest.FunSuite

class PPUSpriteHitSuite extends FunSuite {
  test("Can run ppu_sprite_hit/01-basics test ROM") {
    Util.runTestROM(NESFile.fromFile(new File("test-roms/ppu_sprite_hit/rom_singles/01-basics.nes")))
  }

  test("Can run ppu_sprite_hit/02-alignment test ROM") {
    Util.runTestROM(NESFile.fromFile(new File("test-roms/ppu_sprite_hit/rom_singles/02-alignment.nes")))
  }

  test("Can run ppu_sprite_hit/03-corners test ROM") {
    Util.runTestROM(NESFile.fromFile(new File("test-roms/ppu_sprite_hit/rom_singles/03-corners.nes")))
  }

  test("Can run ppu_sprite_hit/04-flip test ROM") {
    Util.runTestROM(NESFile.fromFile(new File("test-roms/ppu_sprite_hit/rom_singles/04-flip.nes")))
  }

  test("Can run ppu_sprite_hit/05-left_clip test ROM") {
    Util.runTestROM(NESFile.fromFile(new File("test-roms/ppu_sprite_hit/rom_singles/05-left_clip.nes")))
  }

  test("Can run ppu_sprite_hit/06-right_edge test ROM") {
    Util.runTestROM(NESFile.fromFile(new File("test-roms/ppu_sprite_hit/rom_singles/06-right_edge.nes")))
  }

  // test("Can run ppu_sprite_hit/07-screen_bottom test ROM") {
  //   Util.runTestROM(NESFile.fromFile(new File("test-roms/ppu_sprite_hit/rom_singles/07-screen_bottom.nes")))
  // }

  test("Can run ppu_sprite_hit/08-double_height test ROM") {
    Util.runTestROM(NESFile.fromFile(new File("test-roms/ppu_sprite_hit/rom_singles/08-double_height.nes")))
  }

  // test("Can run ppu_sprite_hit/09-timing test ROM") {
  //   Util.runTestROM(NESFile.fromFile(new File("test-roms/ppu_sprite_hit/rom_singles/09-timing.nes")))
  // }

  // test("Can run ppu_sprite_hit/10-timing_order test ROM") {
  //   Util.runTestROM(NESFile.fromFile(new File("test-roms/ppu_sprite_hit/rom_singles/10-timing_order.nes")))
  // }
}
