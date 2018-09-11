package me.shadaj.nessie

import java.io.File

import org.scalatest.FunSuite

class BranchTimingSuite extends FunSuite {
  test("branch_timing_tests/1.Branch_Basics") {
    Util.checkNthFrame(
      NESFile.fromFile(new File("test-roms/branch_timing_tests/1.Branch_Basics.nes")), 13,
      new File("test-frames/branch_timing_tests/1.Branch_Basics.png")
    )
  }

  test("branch_timing_tests/2.Backward_Branch") {
    Util.checkNthFrame(
      NESFile.fromFile(new File("test-roms/branch_timing_tests/2.Backward_Branch.nes")), 16,
      new File("test-frames/branch_timing_tests/2.Backward_Branch.png")
    )
  }

  test("branch_timing_tests/3.Forward_Branch") {
    Util.checkNthFrame(
      NESFile.fromFile(new File("test-roms/branch_timing_tests/3.Forward_Branch.nes")), 15,
      new File("test-frames/branch_timing_tests/3.Forward_Branch.png")
    )
  }
}
