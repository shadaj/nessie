package me.shadaj.nessie

import java.awt.{Color, Graphics}
import java.awt.event.{KeyEvent, KeyListener}
import java.io.File

import javax.swing.{JFrame, JPanel}
import scala.io.StdIn

object RunGame extends App {
  var currentFrame: Array[Array[(Int, Int, Int)]] = null
  val scale = 3

  val panel = new JPanel() {
    override def paintComponent(g: Graphics): Unit = {
      if (currentFrame != null) {
        (8 until 232).foreach { y => // NTSC drops top and bottom tiles
          (0 until 256).foreach { x =>
            val pixel = currentFrame(y)(x)
            g.setColor(new Color(pixel._1, pixel._2, pixel._3))
            g.fillRect(x * scale, (y - 8) * scale, scale, scale)
          }
        }
      }
    }
  }

  val frame: JFrame = new JFrame() {
    add(panel)
  }

  frame.setSize(256 * scale, 224 * scale) // NTSC is 224
  frame.setVisible(true)
  val buttonsPressed = new Array[Boolean](8)
  val buttonsMap = Seq(
    KeyEvent.VK_ALT, KeyEvent.VK_META, KeyEvent.VK_SPACE, KeyEvent.VK_ENTER,
    KeyEvent.VK_UP, KeyEvent.VK_DOWN, KeyEvent.VK_LEFT, KeyEvent.VK_RIGHT
  ).zipWithIndex.toMap

  frame.addKeyListener(new KeyListener {
    override def keyTyped(e: KeyEvent): Unit = {}

    override def keyPressed(e: KeyEvent): Unit = {
      buttonsMap.get(e.getKeyCode).foreach { i =>
        buttonsPressed(i) = true
      }
    }

    override def keyReleased(e: KeyEvent): Unit = {
      buttonsMap.get(e.getKeyCode).foreach { i =>
        buttonsPressed(i) = false
      }
    }
  })

  val apu = new APU

  val console = new Console(NESFile.fromFile(new File(args.head)), f => {
    currentFrame = f
  }, () => buttonsPressed.toVector)

  val nanoPeriod = (1000L * 1000 * 1000) / 60
  while (true) {
    val startTime = System.nanoTime()
    panel.paintImmediately(0, 0, 256 * scale, 240 * scale) // draw the frame that was loaded up in the last loop
    while (!console.tick()) {} // load up a frame
    while ((System.nanoTime() - startTime) < nanoPeriod) {} // wait until it's time to display the frame
  }
}
