package io.github.edadma.texish

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

  "the page background honours backgroundColor" in {
    // a dark page colour fills the page instead of white; the default leaves the white paper that
    // print output expects
    val t = new Graphics2DTypesetter(dpi = 36)
    t.backgroundColor = Color("black")

    Console.withOut(new java.io.ByteArrayOutputStream) {
      t.add(new CharBox(t, "x"))
      t.end()
    }

    val img    = t.getDocument.printedPages.head.asInstanceOf[BufferedImage]
    val corner = img.getRGB(0, 0)
    (corner & 0xffffff) shouldBe 0x000000   // bare page is black, not white
    (corner >>> 24) shouldBe 0xff           // and opaque
  }

  private def luminance(rgb: Int): Int =
    val r = (rgb >> 16) & 0xff
    val g = (rgb >> 8) & 0xff
    val b = rgb & 0xff
    (r + g + b) / 3

  "a hairline rule is visible on the page" in {
    // the footnote separator is a 0.4pt rule: thinner than a device pixel at screen resolutions, so it must be
    // filled fractionally (antialiased coverage), not truncated to an integer height of zero
    val t = new Graphics2DTypesetter(dpi = 100)

    Console.withOut(new java.io.ByteArrayOutputStream) {
      t.add(new CharBox(t, "body"))
      t.add(RuleBox(t, 2 * t.in, 0.4, 0))
      t.end()
    }

    val img = t.getDocument.printedPages.head.asInstanceOf[BufferedImage]

    // some row must carry a contiguous ink run as long as the rule — far longer than any word
    val runLength = 1.8 * t.in * 100 / 72

    val visible =
      (0 until img.getHeight).exists { y =>
        var run  = 0
        var best = 0
        for x <- 0 until img.getWidth do
          if luminance(img.getRGB(x, y)) < 230 then
            run += 1
            best = best max run
          else run = 0
        best >= runLength
      }

    assert(visible, "the 0.4pt rule left no visible ink")
  }

  "glyphs are drawn at the widths the engine measured" in {
    // text is measured with fractional metrics; drawing must use them too, or integer glyph advances
    // accumulate across a word and the drawn text drifts off the measured positions, swallowing the
    // interword space after it
    val t = new Graphics2DTypesetter(dpi = 100)

    t.selectFont("noto", 11.2, Set("regular"))

    val text     = "mwiamwiamwiamwiamwia"
    val box      = new CharBox(t, text)
    val measured = box.width * 100 / 72

    Console.withOut(new java.io.ByteArrayOutputStream) {
      t.add(box)
      t.end()
    }

    val img = t.getDocument.printedPages.head.asInstanceOf[BufferedImage]

    var minX = Int.MaxValue
    var maxX = Int.MinValue
    for
      y <- 0 until img.getHeight
      x <- 0 until img.getWidth
    do
      if luminance(img.getRGB(x, y)) < 200 then
        minX = minX min x
        maxX = maxX max x

    val drawn = maxX - minX + 1

    drawn.toDouble shouldBe measured +- 3.0
  }
