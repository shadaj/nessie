package me.shadaj.nessie

import java.io.File

import org.scalatest.FunSuite

import scala.io.Source

class CPUSuite extends FunSuite {
  private val nestest = NESFile.fromFile(new File("test-roms/nestest.nes"))
  private val basics = NESFile.fromFile(new File("test-roms/01-basics.nes"))
  private val officialOnly = NESFile.fromFile(new File("test-roms/official_only.nes"))

  test("Can execute some instructions from basic test ROM and report cycle count") {
    val memory = new Memory(Seq(new NESRam, new PPU(null, null, _ => {}).cpuMemoryMapping, new Mapper0(basics.programRom)))
    val cpu = new CPU(memory)
    assert(cpu.tick == 2) // SEI
    assert(cpu.tick == 3) // JMP $EB12
    assert(cpu.tick == 4) // STA $224
    assert(cpu.tick == 2) // LDA #0
    assert(cpu.tick == 4) // STA $2000
    assert(cpu.tick == 4) // STA $2001
  }

  test("Can run nestest ROM through invalid opcodes (0x04)") {
    val memory = new Memory(Seq(new NESRam, new PPU(null, null, _ => {}).cpuMemoryMapping, new Mapper0(nestest.programRom)))
    val cpu = new CPU(memory)

    cpu.programCounter = 0xC000

    val expectedLines = Source.fromFile("test-roms/nestest.log").getLines()

    var stopRunning = false
    var ticks = 0
    while (!stopRunning) {
      val currentLogLine = expectedLines.next()
      val split = currentLogLine.split(' ').filterNot(_.isEmpty)

      assert(cpu.programCounter == Integer.parseInt(split.head, 16))
      assert(java.lang.Byte.toUnsignedInt(cpu.accumulator) == Integer.parseInt(split.find(_.startsWith("A:")).get.drop(2), 16))
      assert(java.lang.Byte.toUnsignedInt(cpu.xRegister) == Integer.parseInt(split.find(_.startsWith("X:")).get.drop(2), 16))
      assert(java.lang.Byte.toUnsignedInt(cpu.yRegister) == Integer.parseInt(split.find(_.startsWith("Y:")).get.drop(2), 16))
      assert(java.lang.Byte.toUnsignedInt(cpu.stackPointer) == Integer.parseInt(split.find(_.startsWith("SP:")).get.drop(3), 16))

      assert((ticks * 3 % 341) == split.last.split(':').last.toInt)

      if (cpu.memory.read(cpu.programCounter) == 0xA3.toByte) {
        stopRunning = true
      } else {
        ticks += cpu.tick(true)
      }
    }
  }

  def runTestROM(file: NESFile) = {
    var isDone = false
    var success = false
    var message = ""

    val console = new Console(file, _ => {}, () => Seq.fill(5)(false), Seq(
      new MemoryProvider { // test ROMs write the result text here
        private val stringMemory = new Array[Byte](256)

        override def canReadAt(address: Int): Boolean = false
        override def canWriteAt(address: Int): Boolean = address >= 0x6000

        override def read(address: Int, memory: Memory): Byte = ???

        override def write(address: Int, value: Byte, memory: Memory): Unit = {
          if (address >= 0x6004) {
            stringMemory(address - 0x6004) = value
          }

          if (address == 0x6000 && value != -128) {
            message = Iterator.from(0).map(stringMemory.apply).takeWhile(_ != 0).map(_.toChar).mkString
            isDone = true
            success = value == 0
          }
        }
      }
    ))

    while (!isDone) {
      console.tick()
    }

    println(message)
    assert(success)
  }

  test("Can run 01-basics test ROM") {
    runTestROM(NESFile.fromFile(new File("test-roms/01-basics.nes")))
  }

  test("Can run 02-implied test ROM") {
    runTestROM(NESFile.fromFile(new File("test-roms/02-implied.nes")))
  }

  test("Can run official_only test ROM") {
    runTestROM(NESFile.fromFile(new File("test-roms/official_only.nes")))
  }

  test("Can run ppu_sprite_hit/01-basics test ROM") {
    runTestROM(NESFile.fromFile(new File("test-roms/ppu_sprite_hit/rom_singles/01-basics.nes")))
  }

  test("Can run ppu_sprite_hit/02-alignment test ROM") {
    runTestROM(NESFile.fromFile(new File("test-roms/ppu_sprite_hit/rom_singles/02-alignment.nes")))
  }

  test("Can run ppu_sprite_hit/03-corners test ROM") {
    runTestROM(NESFile.fromFile(new File("test-roms/ppu_sprite_hit/rom_singles/03-corners.nes")))
  }

  test("Can run ppu_sprite_hit/04-flip test ROM") {
    runTestROM(NESFile.fromFile(new File("test-roms/ppu_sprite_hit/rom_singles/04-flip.nes")))
  }

  test("Can run ppu_sprite_hit/05-left_clip test ROM") {
    runTestROM(NESFile.fromFile(new File("test-roms/ppu_sprite_hit/rom_singles/05-left_clip.nes")))
  }

  test("Can run ppu_sprite_hit/06-right_edge test ROM") {
    runTestROM(NESFile.fromFile(new File("test-roms/ppu_sprite_hit/rom_singles/06-right_edge.nes")))
  }

//  test("Can run ppu_sprite_hit/07-screen_bottom test ROM") {
//    runTestROM(NESFile.fromFile(new File("test-roms/ppu_sprite_hit/rom_singles/07-screen_bottom.nes")))
//  }
}
