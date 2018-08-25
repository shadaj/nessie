package me.shadaj.nessie

import scala.collection.mutable

case class Sprite(xPosition: Int, yPosition: Int, patternIndex: Int, attributes: Int) {
  def isVisible = yPosition < 0xEF
  def contains(x: Int, y: Int) =
    x >= xPosition && x < (xPosition + 8) && y >= yPosition && y < (yPosition + 8)
}

class PPU(runNMI: () => Unit, ppuMappedMemory: MemoryProvider, drawFrame: Array[Array[(Int, Int, Int)]] => Unit) {
  private var oamAddress: Int = 0x0
  private var nmiOnBlank: Boolean = false
  private var incrementAddressDown: Boolean = false
  private var currentPPUAddr: Either[Byte, Int] = Right(0)
  private var currentScroll: Either[Byte, (Byte, Byte)] = Right((0, 0))
  private var currentOamData: Vector[Byte] = Vector.fill[Byte](64 * 4)(0)
  private var currentSprites: List[Sprite] = List.empty
  private var backgroundPatternTable1 = false

  private val nametableA = Array.fill[Byte](30, 32)(0)
  private val attributeA = Array.fill[Byte](8, 8)(0)

  var universalBackgroundColor = 0

  val paletteMemory = new Array[Byte](32)

  def updateSprites(): Unit = {
    currentSprites = currentOamData.grouped(4).map { spriteData =>
      Sprite(
        java.lang.Byte.toUnsignedInt(spriteData.last),
        java.lang.Byte.toUnsignedInt(spriteData.head),
        java.lang.Byte.toUnsignedInt(spriteData(1)),
        java.lang.Byte.toUnsignedInt(spriteData(2))
      )
    }.filter(_.isVisible).toList
  }

  val cpuMemoryMapping = new MemoryProvider {
    override def contains(address: Int): Boolean = (address >= 0x2000 && address < 0x4000) || address == 0x4014

    override def read(address: Int, memory: Memory): Byte = {
      val actualAddress = (address - 0x2000) % 8 /* mirroring! */
      actualAddress match {
        case 0x2 => 0xff.toByte
        case 0x7 =>
          println(s"trying to read ${currentPPUAddr.right.get}")
          0x0.toByte
      }
    }

    override def write(address: Int, value: Byte, memory: Memory): Unit = {
      val actualAddress = (address - 0x2000) % 8 /* mirroring! */
      if (address == 0x4014) {
        currentOamData = (combineBytes(value, 0) to combineBytes(value, 0xFF.toByte)).map(memory.read).toVector
        updateSprites()
      } else {
        actualAddress match {
          case 0x0 =>
            nmiOnBlank = ((value >>> 7) & 1) == 1
            backgroundPatternTable1 = ((value >>> 4) & 1) == 1
            incrementAddressDown = ((value >>> 2) & 1) == 1
          case 0x1 =>

          case 0x3 =>
            oamAddress = java.lang.Byte.toUnsignedInt(value)

          case 0x4 =>
            currentOamData = currentOamData.updated(oamAddress, value)
            updateSprites()
            oamAddress = (oamAddress + 1) % 256
          case 0x5 =>
            if (currentScroll.isLeft) {
              currentScroll = Right((currentScroll.left.get, value))
            } else {
              currentScroll = Left(value)
            }
          case 0x6 =>
            if (currentPPUAddr.isLeft) {
              currentPPUAddr = Right(combineBytes(currentPPUAddr.left.get, value)).right.map(_ % 0x4000)
            } else {
              currentPPUAddr = Left(value)
            }
          case 0x7 =>
            val addr = currentPPUAddr.right.get

            if (addr >= 0x2000 && addr < (0x2000 + (30 * 32))) {
              val relative = addr - 0x2000
              nametableA(relative / 32)(relative % 32) = value
            } else if (addr >= (0x2000 + (30 * 32)) && addr < 0x2400) {
              val relative = addr - (0x2000 + (30 * 32))
              attributeA(relative / 8)(relative % 8) = value
            } else if (addr == 0x3F00) {
              universalBackgroundColor = value
            } else if (addr >= 0x3F00 && addr < 0x3F20) {
              paletteMemory(addr - 0x3F00) = value
            } else {
              println(f"data at ${currentPPUAddr.right.get}%X = $value%X")
            }

            currentPPUAddr = Right(currentPPUAddr.right.get + (if (incrementAddressDown) 32 else 1)).right.map(_ % 0x4000)
        }
      }
    }
  }

  private var currentLine = -1 // dummy scanline
  private var currentX = 0
  private val currentImage = Array.fill(240, 256)((0, 0, 0))

  val nesToRGB = """ 84  84  84    0  30 116    8  16 144   48   0 136   68   0 100   92   0  48   84   4   0   60  24   0   32  42   0    8  58   0    0  64   0    0  60   0    0  50  60    0   0   0 0 0 0 0 0 0
                   |152 150 152    8  76 196   48  50 236   92  30 228  136  20 176  160  20 100  152  34  32  120  60   0   84  90   0   40 114   0    8 124   0    0 118  40    0 102 120    0   0   0 0 0 0 0 0 0
                   |236 238 236   76 154 236  120 124 236  176  98 236  228  84 236  236  88 180  236 106 100  212 136  32  160 170   0  116 196   0   76 208  32   56 204 108   56 180 204   60  60  60 0 0 0 0 0 0
                   |236 238 236  168 204 236  188 188 236  212 178 236  236 174 236  236 174 212  236 180 176  228 196 144  204 210 120  180 222 120  168 226 144  152 226 180  160 214 228  160 162 160 0 0 0 0 0 0"""
    .stripMargin.split('\n').flatMap(_.split(' ')).filterNot(_.isEmpty).map(_.toInt)
    .grouped(3).map(a => (a(0), a(1), a(2))).toArray

  def readPattern(baseTable: Int, index: Int, x: Int, y: Int, flipVertical: Boolean = false, flipHorizontal: Boolean = false): Int = {
    val xInPattern = if (flipHorizontal) 7 - x else x
    val yInPattern = if (flipVertical) 7 - y else y
    val planeOne = ppuMappedMemory.read(baseTable + (index * 16) + yInPattern, null)
    val planeTwo = ppuMappedMemory.read(baseTable + (index * 16) + 8 + yInPattern, null)
    (((planeTwo >>> (7 - xInPattern)) & 1) << 1) | ((planeOne >>> (7 - xInPattern)) & 1)
  }

  def getSpritePixelAt(x: Int, y: Int) = {
    currentSprites.find(_.contains(x, y)).flatMap { s =>
      val shouldFlipVertically = ((s.attributes >>> 7) & 1) == 1
      val shouldFlipHorizontally = ((s.attributes >>> 6) & 1) == 1
      val relativePixelX = x - s.xPosition
      val relativePixelY = y - s.yPosition

      val paletteIndex = readPattern(
        0x0, s.patternIndex,
        relativePixelX, relativePixelY,
        shouldFlipVertically, shouldFlipHorizontally
      )

      val basePaletteAddress = 0x10 + ((s.attributes % 4) << 2)

      if (paletteIndex != 0) {
        Some(nesToRGB(paletteMemory(basePaletteAddress + paletteIndex)))
      } else {
        None
      }
    }
  }

  var lastFrameTime = System.currentTimeMillis()

  def getBackgroundPixelAt(x: Int, y: Int) = {
    val tileIndexX = x / 8
    val tileIndexY = y / 8
    val relativePixelX = x % 8
    val relativePixelY = y % 8

    val patternTileNumber = java.lang.Byte.toUnsignedInt(nametableA(tileIndexY)(tileIndexX))
    val paletteIndex = readPattern(
      if (backgroundPatternTable1) 0x1000 else 0x0,
      patternTileNumber,
      relativePixelX,
      relativePixelY
    )

    if (paletteIndex != 0) {
      val attributeValue = attributeA(tileIndexY / 4)(tileIndexX / 4)
      val xSide = (tileIndexX % 4) / 2
      val ySide = (tileIndexY % 4) / 2
      val shiftNeeded = (xSide + (ySide * 2)) * 2
      val basePaletteAddress = (java.lang.Byte.toUnsignedInt((attributeValue >>> shiftNeeded).toByte) % 4) << 2
      Some(nesToRGB(paletteMemory(basePaletteAddress | paletteIndex)))
    } else {
      None
    }
  }

  def step(): Boolean = {
    if (currentLine >= 0 && currentLine < 240 && currentX >= 1 && currentX <= 256) {
      val pixelX = currentX - 1
      val pixelY = currentLine
      val color =
        getSpritePixelAt(pixelX, pixelY).
          orElse(getBackgroundPixelAt(pixelX, pixelY))
          .getOrElse(nesToRGB(universalBackgroundColor))

      currentImage(pixelY)(pixelX) = color
    }

    val didDraw = if (currentLine == 240 && currentX == 0) {
      drawFrame(currentImage)

      lastFrameTime = System.currentTimeMillis()

      if (nmiOnBlank) {
        runNMI()
      }

      true
    } else false

    currentX += 1
    if (currentX > 340) {
      currentLine += 1
      currentX = 0
    }

    if (currentLine > 261) { // 261 is a dummy scanline
      currentLine = -1
    }

    didDraw
  }
}
