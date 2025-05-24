package io.github.edadma.typesetter

import pprint.pprintln

import java.awt.image.BufferedImage
import scala.swing.*

object Main extends SimpleSwingApplication:
  def top: Frame = new MainFrame:
    title = "Simple Swing Example"

    contents = new Panel {
      preferredSize = new Dimension(800, 500)

      override def paintComponent(g: Graphics2D): Unit = {
        super.paintComponent(g)

        val t = new Graphics2DTypesetter()

        //        t.hbox(t.getNumber("hsize"))
        //          .addFil()
        //          .add("Hello")
        //          .add(" ")
        //          .add("Scriptura!")
        //          .add(" ")
        //          .add("Cool")
        //          .addFil()
        //          .done()

        //        t.hbox(t.getNumber("hsize"))
        //          .addFil()
        //          .add(
        //            ImageBox(
        //              t,
        //              "866-536x354.jpg",
        //            ),
        //          )
        //          .addFil()
        //          .done()

        //        t.hbox(t.getNumber("hsize"))
        //          .addFil()
        //          .add("one")
        //          .addFil()
        //          .done()

        //        t.hbox(t.getNumber("hsize"))
        //          .fil
        //          .add("[two]")
        //          .fil
        //          .done()
        //
        //        t.hbox(t.getNumber("hsize"))
        //          .fil
        //          .add("[three]")
        //          .fil
        //          .done()

        //        t.start()
        //        t add "asdf"
        //
        //        for _ <- 1 to 20 do
        //          t add " "
        //          t add "zxcv"
        //
        //        t.paragraph()
        //        t.fil

        t.halign
          .add("(").op("placeholder").add(")").add(" ").fil
          .op("newColumn").add("[").op("placeholder").add("]").fil
          .op("newLine")
          .add("asdf").op("newColumn").add("zxcv")
          .op("newLine").op("noalign-begin").add("noalign").op("noalign-end")
          .add("asdf").add(" ").add("1").op("newColumn").add("zxcv 1")

//        t.hbox().add("[one]").done()
//        t.hbox().add("[two]").done()
//        t.hbox().add("[three]").done()

        t.end()
        g.drawImage(t.document.printedPages.head.asInstanceOf[BufferedImage], null, 0, 0)
      }
    }

    override def closeOperation(): Unit = dispose()
