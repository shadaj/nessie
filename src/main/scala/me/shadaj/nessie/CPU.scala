package me.shadaj.nessie

class CPU(val memory: Memory) {
  var accumulator: Byte = 0
  var xRegister: Byte = 0
  var yRegister: Byte = 0
  var programCounter: Int = memory.readTwoBytes(0xFFFC)
  var stackPointer: Byte = 0xFF.toByte

  // processor status
  var carryFlag = false
  var zeroFlag = false
  var interruptDisable = false
  var decimalMode = false
  var overflowFlag = false
  var negativeFlag = false

  def tick(log: Boolean): Int = {
    val currentInstruction = Instruction.cpuInstructions.getOrElse(memory.read(programCounter), {
      throw new IllegalArgumentException(s"Unknown instruction: ${memory.read(programCounter).formatted("0x%x")}")
    })

    val currentCounter = programCounter
    print(programCounter.formatted("%x").toUpperCase + " ")
    programCounter += currentInstruction.argsSize + 1
    val cyclesCount = currentInstruction.run(i => memory.read(currentCounter + 1 + i), log)(this)
    cyclesCount
  }

  def tick: Int = tick(false)
}
