package io.github.edadma.typesetter

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import java.awt.image.BufferedImage

class Graphics2DPageTests extends AnyFreeSpec with Matchers:

  private def pixels(img: BufferedImage): Array[Int] =
    img.getRGB(0, 0, img.getWidth, img.getHeight, null, 0, img.getWidth)

  "each shipped page is a separate image with its own content" in {
    val t = new Graphics2DTypesetter(dpi = 36)

    // the pages hold no glue, so shipping each one reports it off-size — irrelevant here
    Console.withOut(new java.io.ByteArrayOutputStream) {
      t.add(new CharBox(t, "one"))
      t.newpage()
      t.add(new CharBox(t, "two"))
      t.end()
    }

    val pages = t.getDocument.printedPages.toList.asInstanceOf[List[BufferedImage]]

    pages.length shouldBe 2
    assert(pages(0) ne pages(1), "pages must not alias the same image")
    pixels(pages(0)) should not equal pixels(pages(1))
  }
