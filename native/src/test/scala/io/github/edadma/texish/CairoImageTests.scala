package io.github.edadma.texish

import io.github.edadma.libcairo.Surface
import io.github.edadma.texish.parser.{Processor, TypesetterHandler, registerTypesettingPrimitives}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import scala.scalanative.unsafe.*

/** The native image-surface backend renders each page onto its own ARGB32 surface, collected in the document's
  * printedPages, sized to the paper dimensions times the device scale.
  */
class CairoImageTests extends AnyFreeSpec with Matchers:

  private def render(dpi: Double, script: String): List[Surface] =
    val t       = new CairoImageTypesetter(dpi)
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)

    registerTypesettingPrimitives(proc, handler)
    proc.process(script)
    t.end()
    t.getDocument.printedPages.toList.map(_.asInstanceOf[Surface])

  /** Number of pixels on the page that are not opaque white — i.e. where something was drawn. */
  private def inkPixels(s: Surface): Int =
    s.flush()

    val data   = s.getData
    val stride = s.getStride
    val w      = s.getWidth
    val h      = s.getHeight
    var count  = 0
    var y      = 0

    while y < h do
      var x = 0

      while x < w do
        val p = data + y.toLong * stride + x.toLong * 4

        if !((p(0) & 0xff) == 0xff && (p(1) & 0xff) == 0xff && (p(2) & 0xff) == 0xff && (p(3) & 0xff) == 0xff) then
          count += 1
        x += 1
      y += 1
    count

  private def whiteCorner(s: Surface): Boolean =
    s.flush()

    val p = s.getData

    (p(0) & 0xff) == 0xff && (p(1) & 0xff) == 0xff && (p(2) & 0xff) == 0xff && (p(3) & 0xff) == 0xff

  "a page surface is sized to the paper dimensions times the device scale" in {
    // letter paper is 8.5x11in = 612x792pt; at 100dpi the device scale is 100/72
    val pages = render(100, "Hello world\n\n")

    pages.length shouldBe 1
    pages.head.getWidth shouldBe 850   // 612 * 100/72
    pages.head.getHeight shouldBe 1100 // 792 * 100/72
  }

  "the device scale tracks dpi" in {
    val pages = render(200, "Hello world\n\n")

    pages.head.getWidth shouldBe 1700  // 612 * 200/72
    pages.head.getHeight shouldBe 2200 // 792 * 200/72
  }

  "each page has an opaque white background and drawn text" in {
    val pages = render(100, "Hello world\n\n")

    whiteCorner(pages.head) shouldBe true
    inkPixels(pages.head) should be > 0
  }

  "a multi-page document ships one surface per page, each with content" in {
    val pages = render(100, "first page\n\n\\vfill\\eject second page\n\n\\vfill\\eject third page\n\n")

    pages.length shouldBe 3
    for page <- pages do
      page.getWidth shouldBe 850
      page.getHeight shouldBe 1100
      inkPixels(page) should be > 0
  }

