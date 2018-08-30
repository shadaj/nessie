package me.shadaj.nessie

class CPU(val memory: Memory) {
  var accumulator: Byte = 0
  var xRegister: Byte = 0
  var yRegister: Byte = 0
  var programCounter: Int = memory.readTwoBytes(0xFFFC)
  var stackPointer: Byte = 0xFD.toByte

  // processor status
  var carryFlag = false
  var zeroFlag = false
  var interruptDisable = false
  var decimalMode = false
  var overflowFlag = false
  var negativeFlag = false

  var interruptAddress: Option[Int] = None

  def atIndex(value: Boolean, index: Int): Byte = {
    ((if (value) 1 else 0) << index).toByte
  }

  def statusRegister = {
    (0
      | atIndex(carryFlag, 0)
      | atIndex(zeroFlag, 1)
      | atIndex(interruptDisable, 2)
      | atIndex(decimalMode, 3)
      | atIndex(true, 4)
      | atIndex(true, 5)
      | atIndex(overflowFlag, 6)
      | atIndex(negativeFlag, 7)).toByte
  }

  def tick(log: Boolean): Int = {
    val handleInterruptCycles = interruptAddress.map { a =>
      interruptAddress = None
      val addressToPush = programCounter
      Instruction.pushToStack((addressToPush >> 8).toByte, this)
      Instruction.pushToStack((addressToPush & 0xFF).toByte, this)

      Instruction.pushToStack(statusRegister, this)

      programCounter = a
      interruptDisable = true

      7
    }.getOrElse(0)

    val instructionOpcode = memory.read(programCounter)
    val (currentInstruction, parser) = Instruction.cpuInstructions.getOrElse(instructionOpcode, {
      throw new IllegalArgumentException(s"Unknown instruction: ${memory.read(programCounter).formatted("0x%x")}")
    })

    val currentCounter = programCounter
    if (log) {
      print(programCounter.formatted("%x").toUpperCase + " ")
    }
    programCounter += parser.size + 1
    val parsedArgs = parser.parse(i => memory.read(currentCounter + 1 + i), this)
    handleInterruptCycles + currentInstruction.run(parsedArgs, log)(this)
  }

  def tick: Int = tick(false)

  def runNMI: Unit = {
    interruptAddress = Some(memory.readTwoBytes(0xFFFA))
  }
}
