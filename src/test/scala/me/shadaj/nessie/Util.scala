package me.shadaj.nessie

import java.awt.image.BufferedImage
import java.io.File

import javax.imageio.ImageIO
import org.scalatest.Assertions

object Util {
  def checkNthFrame(file: NESFile, frame: Int, expectedImage: File, memoryProviders: Seq[MemoryProvider] = Seq.empty, update: Boolean = false) = {
    var currentFrame: Array[Array[(Int, Int, Int)]] = null
    val console = new Console(file, a => {
      currentFrame = a
    }, () => Seq.fill(5)(false), memoryProviders)

    (1 to frame).foreach { f =>
      while (!console.tick()) {}
    }

    if (update || !expectedImage.exists()) {
      println("Updating frame snapshot")

      if (!expectedImage.exists()) {
        expectedImage.mkdirs()
        expectedImage.createNewFile()
      }

      val image = new BufferedImage(currentFrame.head.length, currentFrame.length, BufferedImage.TYPE_INT_RGB)
      val raster = image.getRaster
      currentFrame.zipWithIndex.foreach { case (row, y) =>
        row.zipWithIndex.foreach { case (pixel, x) =>
          raster.setPixel(x, y, Array(pixel._1, pixel._2, pixel._3))
        }
      }

      ImageIO.write(image, "png", expectedImage)
    } else {
      val existingImage = ImageIO.read(expectedImage)
      val raster = existingImage.getRaster
      currentFrame.zipWithIndex.foreach { case (row, y) =>
        row.zipWithIndex.foreach { case (pixel, x) =>
          val Array(imageR, imageG, imageB) = raster.getPixel(x, y, null: Array[Int])
          Assertions.assert(pixel._1 == imageR && pixel._2 == imageG && pixel._3 == imageB)
        }
      }
    }
  }

  def runTestROM(file: NESFile, log: Boolean = false) = {
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
      console.tick(log)
    }

    println(message)
    Assertions.assert(success)
  }
}
