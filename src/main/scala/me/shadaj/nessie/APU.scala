package me.shadaj.nessie

import javax.sound.sampled.{AudioFormat, AudioSystem}
import scala.util.Try

class APU extends MemoryProvider {
  val sampleRate = 44100
  val format = new AudioFormat(44100, 8, 1, true, false)
  val line = Try(AudioSystem.getSourceDataLine(format)).toOption

  val periodPerSample = 1.0 / sampleRate

  val samplesPerFrame = Math.ceil(sampleRate / 60).toInt
  line.foreach(_.open(format, samplesPerFrame * 4))
  line.foreach(_.start())

  override def canReadAt(address: Int): Boolean = address >= 0x4000 && address < 0x4018 && address != 0x4014 && address != 0x4016 && address != 0x4017
  override def canWriteAt(address: Int): Boolean = address >= 0x4000 && address < 0x4018 && address != 0x4014 && address != 0x4016

  override def read(address: Int, memoryAccess: Memory): Byte = {
    0
  }

  val counterMap = Array(
    10,254, 20,  2, 40,  4, 80,  6, 160,  8, 60, 10, 14, 12, 26, 14,
    12, 16, 24, 18, 48, 20, 96, 22, 192, 24, 72, 26, 16, 28, 32, 30
  )

  var pulse1ConstantVolume = false
  var pulse1Volume = 0
  var pulse1Timer = 0
  var pulse1LengthCounter = 0
  var pulse1CounterHalt = false
  
  var pulse2ConstantVolume = false
  var pulse2Volume = 0
  var pulse2Timer = 0
  var pulse2LengthCounter = 0
  var pulse2CounterHalt = false
  
  var triangleTimer = 0
  var triangleHaltCounter = 0
  var triangleLinearCounter = 0

  val lengthCounterTicksPerSecond = 120
  val samplesPerLengthCounterTick = sampleRate / lengthCounterTicksPerSecond

  val linearCounterTicksPerSecond = 240
  val samplesPerLinearCounterTick = sampleRate / linearCounterTicksPerSecond
  
  private var i = 0

  def squareWave(timer: Int): Int = {
    val targetFrequency = (1.789773 * 1000000) / (16 * (timer + 1))
    val targetPeriod = 1.0 / targetFrequency //(1.789773 * 1000000) / (16 * targetFrequency1) - 1
    if (targetPeriod > periodPerSample) {
      val periodInSamples = (targetPeriod / periodPerSample).toInt
      val periodInSamplesHigh = periodInSamples / 2
      if ((i % periodInSamples) < periodInSamplesHigh) 1 else -1
    } else 0
  }

  // https://wiki.nesdev.com/w/index.php/APU_Triangle
  val triangleSequence = Array(
    15, 14, 13, 12, 11, 10,  9,  8,  7,  6,  5,  4,  3,  2,  1,  0,
    0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15
  ).map(v => (v.toDouble - 7.5) / 8)
  
  def triangleWave(timer: Int): Double = {
    // ticks at rate of CPU, which is double APU
    val targetFrequency = (1.789773 * 1000000) / (32 * (timer + 1))
    val targetPeriod = 1.0 / targetFrequency
    if (targetPeriod > periodPerSample) {
      val periodInSamples = (targetPeriod / periodPerSample).toInt
      val relativeToPeriod = i % periodInSamples
      val currentTick = (relativeToPeriod * 32 / periodInSamples) min 31
      triangleSequence(currentTick)
    } else 0
  }

  def boop(): Unit = {
    val data = (0 until samplesPerFrame).map { _ =>
      i += 1

      ((
        (
          (if (pulse1LengthCounter > 0) {
            val ret = squareWave(pulse1Timer) * (if (pulse1ConstantVolume) {
              pulse1Volume.toDouble / 15
            } else 1)
            
            if (i % samplesPerLengthCounterTick == 0 && !pulse1CounterHalt) {
              pulse1LengthCounter -= 1
            }
            ret
          } else 0) +
          (if (pulse2LengthCounter > 0) {
            val ret = squareWave(pulse2Timer) * (if (pulse2ConstantVolume) {
              pulse2Volume.toDouble / 15
            } else 1)
            
            if (i % samplesPerLengthCounterTick == 0 && !pulse2CounterHalt) {
              pulse2LengthCounter -= 1
            }
            ret
          } else 0) +
          (if (triangleHaltCounter > 0 && triangleLinearCounter > 0) {
            val ret = triangleWave(triangleTimer)
            
            if (i % samplesPerLengthCounterTick == 0) {
              triangleHaltCounter -= 1
            }

            if (i % samplesPerLinearCounterTick == 0) {
              triangleLinearCounter -= 1
            }
            
            ret
          } else 0)
        ).toDouble / 3
        ) * 127).toByte
    }.toArray
    line.foreach(_.write(data, 0, data.length))
  }

  line.foreach { _ =>
    new Thread(() => {
      while (true) {
        boop()
      }
    }).start()
  }
  
  override def write(address: Int, value: Byte, memoryAccess: Memory): Unit = {
    if (address == 0x4000) {
      pulse1ConstantVolume = ((value >>> 4) & 1) == 1
      pulse1Volume = value & 0xF
      pulse1CounterHalt = (value & (1 << 5)) != 0
    } else if (address == 0x4002) {
      pulse1Timer = ((pulse1Timer >>> 8) << 8) + value
    } else if (address == 0x4003) {
      pulse1Timer = (pulse1Timer & 0xFF) + ((value & 7) << 8) // only bottom 3 bits
      pulse1LengthCounter = counterMap(java.lang.Byte.toUnsignedInt(value) >>> 3)
    } else if (address == 0x4004) {
      pulse2ConstantVolume = ((value >>> 4) & 1) == 1
      pulse2Volume = value & 0xF
      pulse2CounterHalt = (value & (1 << 5)) != 0
    } else if (address == 0x4006) {
      pulse2Timer = ((pulse2Timer >>> 8) << 8) + value
    } else if (address == 0x4007) {
      pulse2Timer = (pulse2Timer & 0xFF) + ((value & 7) << 8) // only bottom 3 bits
      pulse2LengthCounter = counterMap(java.lang.Byte.toUnsignedInt(value) >>> 3)
    } else if (address == 0x4008) {
      triangleLinearCounter = (value << 1) >>> 1
    } else if (address == 0x400A) {
      triangleTimer = ((triangleTimer >>> 8) << 8) + value
    } else if (address == 0x400B) {
      triangleTimer = (triangleTimer & 0xFF) + ((value & 7) << 8) // only bottom 3 bits
      triangleHaltCounter = counterMap(java.lang.Byte.toUnsignedInt(value) >>> 3)
    }
  }
}
