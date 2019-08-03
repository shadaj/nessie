package me.shadaj.nessie

import java.io.File

import org.scalatest.FunSuite

class GikoSuite extends FunSuite {
  test("Can run giko005 test ROM") {
    Util.checkNthFrame(
      NESFile.fromFile(new File("test-roms/giko/giko005.nes")), 60 * 2,
      new File("test-frames/giko/giko005.png")
    )
  }

  test("Can run giko011 test ROM") {
    Util.checkNthFrame(
      NESFile.fromFile(new File("test-roms/giko/giko011.nes")), 60 * 2,
      new File("test-frames/giko/giko011-0.png")
    )

    Util.checkNthFrame(
      NESFile.fromFile(new File("test-roms/giko/giko011.nes")), 60 * 4,
      new File("test-frames/giko/giko011-1.png")
    )
  }

  test("Can run giko016 test ROM") {
    (1 to 10).foreach { t =>
      Util.checkNthFrame(
        NESFile.fromFile(new File("test-roms/giko/giko016.nes")), 60 * t,
        new File(s"test-frames/giko/giko016-$t.png")
      )
    }
  }
}
