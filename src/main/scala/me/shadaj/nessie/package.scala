package me.shadaj

package object nessie {
  def combineBytes(byteOne: Byte, byteTwo: Byte): Int = {
    (byteOne.toInt << 8) | java.lang.Byte.toUnsignedInt(byteTwo)
  }
}
