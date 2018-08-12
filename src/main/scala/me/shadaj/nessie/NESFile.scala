package me.shadaj.nessie

import java.io.File
import java.nio.file.Files

import scala.collection.mutable

case class NESFile(programRom: Array[Byte], mapperNumber: Short) {
}

object NESFile {
  // see http://wiki.nesdev.com/w/index.php/INES
  def fromBytes(bytes: Array[Byte]): NESFile = {
    // check start of header: NES + MS-DOS eof
    val header = bytes.slice(0, 16)
    assert(header(0) == 0x4E && header(1) == 0x45 && header(2) == 0x53 && header(3) == 0x1A)
    val prgRomSize = header(4) * 16 * 1024 // stored in 16 KB units

    val flagsSix = header(6)
    val flagsSeven = header(7)

    val mapperNumberLowerNibble = flagsSix >> 4
    val mapperNumberUpperNibble = flagsSeven >> 4
    val mapperNumber = (mapperNumberUpperNibble << 4) | mapperNumberLowerNibble

    val prgRom = bytes.drop(16).take(prgRomSize)

    NESFile(prgRom, mapperNumber.toShort)
  }

  def fromFile(file: File): NESFile = fromBytes(Files.readAllBytes(file.toPath))
}
