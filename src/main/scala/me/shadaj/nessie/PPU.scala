package me.shadaj.nessie

case class Sprite(xPosition: Int, yPosition: Int, patternIndex: Int, attributes: Int) {
  def isVisible: Boolean = yPosition < 240
  def contains(x: Int, y: Int): Boolean =
    x >= xPosition && x < (xPosition + 8) && containsY(y)
  def containsY(y: Int): Boolean =
    y >= yPosition && y < (yPosition + 8)

  def aboveBackground: Boolean = (attributes & 0x20) == 0
}

final class PPU(runNMI: () => Unit, ppuMemory: Memory, drawFrame: Array[Array[(Int, Int, Int)]] => Unit) {
  import PPU._

  private var oamAddress = 0x0
  private var nmiOnBlank = false
  private var incrementAddressDown = false
  private var xScrollShifted = false
  private var yScrollShifted = false
  private var settingPPUHigh = true
  private var currentPPUAddr = 0

  private var vblankFlag = false

  private var settingScrollX = true
  private var currentScrollX = 0
  private var currentScrollY = 0

  private var currentOamData = Vector.fill[Byte](64 * 4)(0)
  private var currentSprites = List.empty[(Sprite, Int)]
  private var spritePatternTable1 = false
  private var backgroundPatternTable1 = false
  private var spriteZeroHit = false

  private var showBackground = false
  private var showBackgroundLeft8 = false
  private var showSpritesLeft8 = false
  private var showSprites = false

  private val paletteMemory = new Array[Byte](32)

  private def universalBackgroundColor = paletteMemory(0)

  private def updateSprites(): Unit = {
    currentSprites = currentOamData.grouped(4).map { spriteData =>
      Sprite(
        java.lang.Byte.toUnsignedInt(spriteData.last),
        java.lang.Byte.toUnsignedInt(spriteData.head) + 1 /* off by one? */,
        java.lang.Byte.toUnsignedInt(spriteData(1)),
        java.lang.Byte.toUnsignedInt(spriteData(2))
      )
    }.zipWithIndex.toList
  }

  private var lastWrite: Byte = 0

  val cpuMemoryMapping: MemoryProvider = new MemoryProvider {
    override def canReadAt(address: Int): Boolean = (address >= 0x2000 && address < 0x4000) || address == 0x4014
    override def canWriteAt(address: Int): Boolean = (address >= 0x2000 && address < 0x4000) || address == 0x4014

    private var vramBuffer: Byte = 0
    override def read(address: Int, memory: Memory): Byte = {
      val actualAddress = (address - 0x2000) % 8 /* mirroring! */
      actualAddress match {
        case 0x1 => 0
        case 0x2 =>
          val ret = ((if (vblankFlag) 1 else 0) << 7 |
           (if (spriteZeroHit) 1 else 0) << 6).toByte

          vblankFlag = false

          // reset latches
          settingScrollX = true
          settingPPUHigh = true

          (ret | (lastWrite & 31).toByte).toByte
        case 0x7 =>
          val addr = currentPPUAddr
          val ret = if (addr < 0x3F00) {
            val retByte = vramBuffer
            vramBuffer = ppuMemory.read(addr)
            retByte
          } else {
            println(f"trying to read $currentPPUAddr%X")
            0x0.toByte
          }
          currentPPUAddr = currentPPUAddr + (if (incrementAddressDown) 32 else 1) % 0x4000
          ret
      }
    }

    override def write(address: Int, value: Byte, memory: Memory): Unit = {
      if (address == 0x4014) {
        currentOamData = (combineBytes(value, 0) to combineBytes(value, 0xFF.toByte)).map(memory.read).toVector
        updateSprites()
      } else {
        val ppuRegister = (address - 0x2000) % 8 /* mirroring! */
        lastWrite = value
        ppuRegister match {
          case 0x0 =>
            nmiOnBlank = ((value >>> 7) & 1) == 1
            backgroundPatternTable1 = ((value >>> 4) & 1) == 1
            spritePatternTable1 = ((value >>> 3) & 1) == 1
            incrementAddressDown = ((value >>> 2) & 1) == 1
            xScrollShifted = ((value >>> 0) & 1) == 1
            yScrollShifted = ((value >>> 1) & 1) == 1

            currentScrollX = (currentScrollX & 0xFF) | (if (xScrollShifted) 256 else 0)
            currentScrollY = (currentScrollY & 0xFF) | (if (yScrollShifted) 256 else 0)
          case 0x1 =>
            showBackgroundLeft8 = ((value >>> 1) & 1) == 1
            showSpritesLeft8 = ((value >>> 2) & 1) == 1
            showBackground = ((value >>> 3) & 1) == 1
            showSprites = ((value >>> 4) & 1) == 1

          case 0x2 => // TODO: why writing here?

          case 0x3 =>
            oamAddress = java.lang.Byte.toUnsignedInt(value)

          case 0x4 =>
            currentOamData = currentOamData.updated(oamAddress, value)
            updateSprites()
            oamAddress = (oamAddress + 1) % 256
          case 0x5 =>
            if (settingScrollX) {
              currentScrollX = (currentScrollX & 0xFF00) | java.lang.Byte.toUnsignedInt(value)
              settingScrollX = false
            } else {
              currentScrollY = (currentScrollY & 0xFF00) | java.lang.Byte.toUnsignedInt(value)
              settingScrollX = true
            }
          case 0x6 =>
            if (settingPPUHigh) {
              currentPPUAddr = combineBytes(value, currentPPUAddr.toByte) % 0x4000
              settingPPUHigh = false
            } else {
              currentPPUAddr = (currentPPUAddr & 0xFF00) | java.lang.Byte.toUnsignedInt(value)
              settingPPUHigh = true

              val addr = currentPPUAddr & 0x07FF
              val xScrollBase = if (addr < 0x400) 0 else 256
              currentScrollX = (currentScrollX & 0xFF) | xScrollBase
            }
          case 0x7 =>
            if (currentPPUAddr < 0x3F00) {
              ppuMemory.write(currentPPUAddr, value)
            } else if (currentPPUAddr >= 0x3F00 && currentPPUAddr < 0x3F20) {
              val relativeAddr = currentPPUAddr - 0x3F00
              val mirroredPalette = if (relativeAddr % 4 == 0) {
                relativeAddr % 0x10
              } else relativeAddr
              paletteMemory(mirroredPalette) = value
            } else {
              println(f"data at ${currentPPUAddr}%X = $value%X")
            }

            currentPPUAddr = currentPPUAddr + (if (incrementAddressDown) 32 else 1) % 0x4000
        }
      }
    }
  }

  private var currentLine = -1 // dummy scanline
  private var currentX = 0
  private val currentImage = Array.fill(240, 256)((0, 0, 0))

  private def readPattern(baseTable: Int, index: Int,
                  x: Int, y: Int,
                  flipVertical: Boolean = false, flipHorizontal: Boolean = false): Int = {
    val xInPattern = if (flipHorizontal) 7 - x else x
    val yInPattern = if (flipVertical) 7 - y else y
    val planeOne = ppuMemory.read(baseTable + (index * 16) + yInPattern)
    val planeTwo = ppuMemory.read(baseTable + (index * 16) + 8 + yInPattern)
    (((planeTwo >>> (7 - xInPattern)) & 1) << 1) | ((planeOne >>> (7 - xInPattern)) & 1)
  }

  private def getSpritePixelAt(x: Int, y: Int,
                               spriteList: List[(Sprite, Int)]): Option[(Int, (Int, Int, Int), Sprite)] = {
    def searchForSprite(list: List[(Sprite, Int)]): Option[(Int, (Int, Int, Int), Sprite)] = {
      if (list.isEmpty) {
        None
      } else if (list.head._1.isVisible && list.head._1.contains(x, y) && list.head._1.yPosition > 1) {
        val s = list.head._1
        val shouldFlipVertically = ((s.attributes >>> 7) & 1) == 1
        val shouldFlipHorizontally = ((s.attributes >>> 6) & 1) == 1
        val relativePixelX = x - s.xPosition
        val relativePixelY = y - s.yPosition

        val paletteIndex = readPattern(
          if (spritePatternTable1) 0x1000 else 0x0, s.patternIndex,
          relativePixelX, relativePixelY,
          shouldFlipVertically, shouldFlipHorizontally
        )

        val basePaletteAddress = 0x10 + ((s.attributes % 4) << 2)

        if (paletteIndex != 0) {
          Some((list.head._2, nesToRGB(paletteMemory(basePaletteAddress + paletteIndex)), list.head._1))
        } else {
          searchForSprite(list.tail)
        }
      } else {
        searchForSprite(list.tail)
      }
    }

    if (!showSprites || (!showSpritesLeft8 && x < 8)) None else {
      searchForSprite(spriteList)
    }
  }

  def getBackgroundPixelAt(x: Int, y: Int): Option[(Int, Int, Int)] = {
    if (!showBackground || (!showBackgroundLeft8 && x < 8)) None else {
      val xWithScroll = (x + currentScrollX) % (256 * 2)
      val yWithScroll = (y + currentScrollY +
        (if ((currentScrollY / 8) >= 31) -8 * 2 else 0) // not sure why, but it works?
      ) % (240 * 2)
      val tileIndexX = xWithScroll / 8
      val tileIndexY = yWithScroll / 8
      val relativePixelX = xWithScroll % 8
      val relativePixelY = yWithScroll % 8

      val (nametable, tileXInTable, tileYInTable) =
        if (tileIndexY < 30) {
          if (tileIndexX < 32) {
            (0x2000, tileIndexX, tileIndexY)
          } else {
            (0x2400, tileIndexX - 32, tileIndexY)
          }
        } else {
          if (tileIndexX < 32) {
            (0x2800, tileIndexX, tileIndexY - 30)
          } else {
            (0x2C00, tileIndexX - 32, tileIndexY - 30)
          }
        }

      val attributeTable = nametable + 0x3C0

      val patternTileNumber = java.lang.Byte.toUnsignedInt(
        ppuMemory.read(nametable + (tileYInTable * 32) + tileXInTable)
      )
      val paletteIndex = readPattern(
        if (backgroundPatternTable1) 0x1000 else 0x0,
        patternTileNumber,
        relativePixelX,
        relativePixelY
      )

      if (paletteIndex != 0) {
        val attributeValue = ppuMemory.read(
          attributeTable + ((tileYInTable / 4) * 8) + (tileXInTable / 4)
        )
        val xSide = (tileIndexX % 4) / 2
        val ySide = (tileIndexY % 4) / 2
        val shiftNeeded = (xSide + (ySide * 2)) * 2
        val basePaletteAddress = (java.lang.Byte.toUnsignedInt((attributeValue >>> shiftNeeded).toByte) % 4) << 2
        Some(nesToRGB(paletteMemory(basePaletteAddress | paletteIndex)))
      } else {
        None
      }
    }
  }

  private var currentLineSpriteList = List.empty[(Sprite, Int)]
  private var evenFrame = false

  def step(): Boolean = {
    val didDraw = if (currentLine == -1) {
      if (currentX == 0) {
        vblankFlag = false
      }

      if (currentX == 1) {
        spriteZeroHit = false
      }

      false
    } else if (currentLine < 240) {
      val pixelX = currentX - 1
      val pixelY = currentLine

      if (currentX == 0) {
        currentLineSpriteList = currentSprites.filter(_._1.containsY(pixelY))
      } else if (currentX >= 1 && currentX <= 256) {
        val spritePixel = getSpritePixelAt(pixelX, pixelY, currentLineSpriteList)
        val color =
          spritePixel
            .map { case (_, pixel, sprite) =>
              if (sprite.aboveBackground) {
                pixel
              } else {
                getBackgroundPixelAt(pixelX, pixelY).getOrElse(
                  pixel
                )
              }
            }
            .orElse(getBackgroundPixelAt(pixelX, pixelY))
            .getOrElse(nesToRGB(universalBackgroundColor))

        if (spritePixel.isDefined && spritePixel.get._1 == 0 && pixelX != 255) {
          if (getBackgroundPixelAt(pixelX, pixelY).isDefined) {
            spriteZeroHit = true
          }
        }

        currentImage(pixelY)(pixelX) = color
      }

      false
    } else if (currentLine == 240 && currentX == 1) {
      vblankFlag = true
      drawFrame(currentImage)

      if (nmiOnBlank) {
        runNMI()
      }

      true
    } else {
      false
    }

    currentX += 1
    if (currentX == 341 || (evenFrame && currentLine == -1 && currentX == 340)) {
      currentLine += 1
      currentX = 0
    }

    if (currentLine == 261) {
      evenFrame = !evenFrame
      currentLine = -1
    }

    didDraw
  }
}

object PPU {
  private val nesToRGB = """ 84  84  84    0  30 116    8  16 144   48   0 136   68   0 100   92   0  48   84   4   0   60  24   0   32  42   0    8  58   0    0  64   0    0  60   0    0  50  60    0   0   0 0 0 0 0 0 0
                           |152 150 152    8  76 196   48  50 236   92  30 228  136  20 176  160  20 100  152  34  32  120  60   0   84  90   0   40 114   0    8 124   0    0 118  40    0 102 120    0   0   0 0 0 0 0 0 0
                           |236 238 236   76 154 236  120 124 236  176  98 236  228  84 236  236  88 180  236 106 100  212 136  32  160 170   0  116 196   0   76 208  32   56 204 108   56 180 204   60  60  60 0 0 0 0 0 0
                           |236 238 236  168 204 236  188 188 236  212 178 236  236 174 236  236 174 212  236 180 176  228 196 144  204 210 120  180 222 120  168 226 144  152 226 180  160 214 228  160 162 160 0 0 0 0 0 0"""
    .stripMargin.split('\n').flatMap(_.split(' ')).filterNot(_.isEmpty).map(_.toInt)
    .grouped(3).map(a => (a(0), a(1), a(2))).toArray
}