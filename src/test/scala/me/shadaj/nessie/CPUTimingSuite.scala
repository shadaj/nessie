package me.shadaj.nessie

import java.io.File

import org.scalatest.FunSuite

class CPUTimingSuite extends FunSuite {
  test("Can run cpu_timing_test6 test ROM") {
    Util.checkNthFrame(
      NESFile.fromFile(new File("test-roms/cpu_timing_test6/cpu_timing_test.nes")), 60 * 11,
      new File("test-frames/cpu_timing_test6/cpu_timing_test.png")
    )
  }
}
