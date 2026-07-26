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

  private def render(dpi: Double, script: String, background: Color = Color("white")): List[Surface] =
    val t       = new CairoImageTypesetter(dpi)
    t.backgroundColor = background
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

  "the default dpi is 72, so one point renders as one pixel" in {
    // with no dpi argument the device scale is the identity: a letter page (612x792pt) is a 612x792px surface,
    // not the 850x1100 the previous 100dpi default produced
    val t       = new CairoImageTypesetter()
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)

    registerTypesettingPrimitives(proc, handler)
    proc.process("Hello world\n\n")
    t.end()

    val page = t.getDocument.printedPages.head.asInstanceOf[Surface]
    page.getWidth shouldBe 612
    page.getHeight shouldBe 792
  }

  "each page has an opaque white background and drawn text" in {
    val pages = render(100, "Hello world\n\n")

    whiteCorner(pages.head) shouldBe true
    inkPixels(pages.head) should be > 0
  }

  "the page background honours backgroundColor" in {
    // A dark page colour (what a dark-scheme screen preview asks for) fills the page instead of
    // white; the default keeps the white paper that print output expects.
    val dark = render(100, "Hello world\n\n", background = Color("black"))

    val p = dark.head
    p.flush()
    val data = p.getData
    // the top-left corner is bare page: opaque, with every colour channel black
    (data(3) & 0xff) shouldBe 0xff
    (data(0) & 0xff) shouldBe 0x00
    (data(1) & 0xff) shouldBe 0x00
    (data(2) & 0xff) shouldBe 0x00

    whiteCorner(render(100, "Hello world\n\n").head) shouldBe true
  }

  "\\pagecolor{transparent} leaves the page clear for compositing over video" in {
    // the lower-third use case: a transparent page ships with only the drawn content opaque, so a compositor
    // shows the video through the bare areas
    val pages = render(100, "\\pagecolor{transparent}\nHello world\n\n")
    val p     = pages.head
    p.flush()
    val data = p.getData
    // the bare top-left corner is fully clear: every channel, alpha included, is zero
    (data(0) & 0xff) shouldBe 0x00
    (data(1) & 0xff) shouldBe 0x00
    (data(2) & 0xff) shouldBe 0x00
    (data(3) & 0xff) shouldBe 0x00
    inkPixels(p) should be > 0 // the text still drew
  }

  "a translucent \\pagecolor tints the whole page at partial alpha" in {
    // a 50%-black band: the bare corner is half-opaque black (premultiplied ARGB32, so the colour channels
    // stay near zero and the alpha byte is about half)
    val pages = render(100, "\\pagecolor[0.5]{black}\nHello world\n\n")
    val p     = pages.head
    p.flush()
    val data = p.getData
    (data(0) & 0xff) shouldBe 0x00
    (data(1) & 0xff) shouldBe 0x00
    (data(2) & 0xff) shouldBe 0x00
    (data(3) & 0xff) shouldBe 128 +- 3
  }

  "the texish home is one of the roots a bundled font is looked for under" in {
    // Each bundled face is named `fonts/…`; Typesetter.home is the directory *containing* that `fonts/` folder,
    // so an embedding application can keep the tree anywhere and point the engine at its parent. The other tests
    // here rely on the current directory finding ./fonts from the source tree; this confirms the configured root
    // is consulted too, and that a root with nothing under it is harmless rather than fatal — the engine falls
    // back to the core compiled into the artifact.
    val original = Typesetter.home
    try
      Typesetter.home = new java.io.File("fonts").getAbsoluteFile.getParent
      new CairoImageTypesetter(100).destroy() // constructed and tore down without a font-load error

      Typesetter.home = "/no/such/texish/home"
      new CairoImageTypesetter(100).destroy()
    finally Typesetter.home = original
  }

  // The engine must work as a plain library dependency, with no font tree anywhere: FreeType opens the Latin
  // Modern core straight from the base64 the artifact carries. FreeType never copies a memory face's bytes and
  // reads them for the life of the face, so the backend keeps them in a malloc'd block — the collection below
  // is what would expose a face left pointing at a Scala array instead.
  "a face opened from the embedded core measures identically to the same face from disk" in {
    val path = "fonts/LatinModernRoman/lmroman10-regular.otf"
    val t    = new CairoImageTypesetter(100)

    try
      val fromDisk  = t.makeFont(t.loadFont(path), 10)
      val fromBytes = t.makeFont(t.loadFontBytes(EmbeddedFonts.get(path).get, path), 10)

      System.gc() // anything the memory face still needed had better not be on the Scala heap

      for c <- "Hamburgefonstiv 3.14" do
        t.glyphIndex(fromBytes, c.toInt) shouldBe t.glyphIndex(fromDisk, c.toInt)
        t.charWidth(fromBytes, c) shouldBe t.charWidth(fromDisk, c)

      t.getTextExtents("Hamburgefonstiv", fromBytes) shouldBe t.getTextExtents("Hamburgefonstiv", fromDisk)
      t.glyphIndex(fromBytes, 'H'.toInt) should not be 0 // a real face, not a bad-parse stub of all .notdef
    finally t.destroy()
  }

  "a multi-page document ships one surface per page, each with content" in {
    val pages = render(100, "first page\n\n\\vfill\\eject second page\n\n\\vfill\\eject third page\n\n")

    pages.length shouldBe 3
    for page <- pages do
      page.getWidth shouldBe 850
      page.getHeight shouldBe 1100
      inkPixels(page) should be > 0
  }

