package me.shadaj.nessie

import java.awt.{Color, Graphics}
import java.awt.event.{KeyEvent, KeyListener}
import java.io.File

import javax.swing.JFrame

object RunGame extends App {
  var currentFrame: Array[Array[(Int, Int, Int)]] = null
  val scale = 3
  val frame: JFrame = new JFrame() {
    override def paint(g: Graphics): Unit = {
      if (currentFrame != null) {
        currentFrame.zipWithIndex.foreach { case (line, y) =>
          line.zipWithIndex.foreach { case (pixel, x) =>
            g.setColor(new Color(pixel._1, pixel._2, pixel._3))
            g.fillRect(x * scale, y * scale, scale, scale)
          }
        }
      }
    }
  }

  frame.setSize(256 * scale, 240 * scale)
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

  val console = new Console(NESFile.fromFile(new File(args.head)), f => {
    currentFrame = f
  }, () => buttonsPressed.toVector)

  val nanoPeriod = (1000 * 1000 * 1000) / 60
  while (true) {
    val startTime = System.nanoTime()
    frame.repaint() // draw the frame that was loaded up in the last loop
    while (!console.tick()) {} // load up a frame
    while ((System.nanoTime() - startTime) < nanoPeriod) {} // wait until it's time to display the frame
  }
}
