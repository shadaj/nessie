package me.shadaj.nessie

import java.io.File

import org.scalatest.FunSuite

class CPUInterruptsSuite extends FunSuite {
  ignore("Can run cpu_interrupts_v2/1-cli_latency test ROM") {
    Util.runTestROM(NESFile.fromFile(new File("test-roms/cpu_interrupts_v2/rom_singles/1-cli_latency.nes")))
  }
}
