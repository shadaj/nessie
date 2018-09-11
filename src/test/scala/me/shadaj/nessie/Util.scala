package me.shadaj.nessie

import java.awt.Color
import java.awt.image.BufferedImage
import java.io.File

import javax.imageio.ImageIO

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
      val graphics = image.getGraphics
      currentFrame.zipWithIndex.foreach { case (row, y) =>
        row.zipWithIndex.foreach { case (pixel, x) =>
          graphics.setColor(new Color(pixel._1, pixel._2, pixel._3))
          graphics.fillRect(x, y, 1, 1)
        }
      }

      ImageIO.write(image, "png", expectedImage)
    } else {
      val existingImage = ImageIO.read(expectedImage)
      val raster = existingImage.getRaster
      currentFrame.zipWithIndex.foreach { case (row, y) =>
        row.zipWithIndex.foreach { case (pixel, x) =>
          val Array(imageR, imageG, imageB) = raster.getPixel(x, y, null: Array[Int])
          assert(imageR == pixel._1 && imageG == pixel._2 && imageB == pixel._3)
        }
      }
    }
  }
}
