package io.github.edadma.texish

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable.ArrayBuffer

/** Regression tests for a set of line-breaking and layout defects found in review: penalty (hyphen) widths
  * charged to lines that merely pass through hyphenation points, the final line judged without
  * `\parfillskip`, glue infinity orders ignored in badness, discardables leaking onto the next line after a
  * break, a broken discretionary's `post` not opening the following line, explicit penalties inert in
  * horizontal lists, `\adjdemerits` unused, the z-fold layout marching off the second sheet, surrogate pairs
  * split by the bidi reorder, and an empty text box crashing the sentence-space rule. All on the fixed-metric
  * [[HeadlessTypesetter]] (glyphs 6 wide, ascent 8, descent 2; default font 14pt).
  */
class LayoutFixesTests extends AnyFreeSpec with Matchers:

  private class W(val width: Double) extends ContentBox:
    val xAdvance: Double = width
    val ascent: Double   = 8
    val descent: Double  = 2
    def draw(t: Typesetter, x: Double, y: Double): Unit = ()
    override def toString: String = s"W($width)"

  "a line is not measured wider for the hyphenation points it passes through" in {
    Hyphenation.enableEmbedded("en-us") shouldBe true
    Hyphenation(Some("en-us"), "hyphenation") should not be None // the word must actually offer break points

    val t = new HeadlessTypesetter
    t.hyphenationLanguage = Some("en-us")
    val sp = t.getGlue("spaceskip")
    // "hyphenation"(66) + space + "x"(6) exactly fills the measure, so it is one perfect line — unless each
    // hyphenation point inside the word wrongly adds a hyphen's width to the measured line
    val boxes  = Seq(new CharBox(t, "hyphenation"), sp, new CharBox(t, "x"))
    val result = KnuthPlass.breakParagraph(boxes, 72.0 + sp.naturalSize, t)

    result shouldBe defined
    result.get.length shouldBe 1
  }

  "a single-word final line is legal — \\parfillskip fills it out" in {
    val t     = new HeadlessTypesetter
    val boxes = Seq(new W(40), Glue(4, 4, 1), new W(40), Glue(4, 4, 1), new W(40))

    // line 1 = 40+4+40 = 84 against 88 (badness 100, within tolerance); the last line is one lone word,
    // which \parfillskip lets fall short at zero badness
    val result = KnuthPlass.breakParagraph(boxes, 88.0, t)

    result shouldBe defined
    result.get.length shouldBe 2
    result.get(1).count(_.isInstanceOf[W]) shouldBe 1
  }

  "a line containing \\hfil is feasible — infinite stretch absorbs the deficit" in {
    val t     = new HeadlessTypesetter
    val boxes = Seq(new W(30), FilGlue, new W(30), Glue(4, 4, 1), new W(60))

    // 30 fil 30 = 60 natural against 70: the fil absorbs the 10 at zero badness; counting fil as one point
    // of finite stretch made this line "infinitely bad" and failed the whole paragraph
    val result = KnuthPlass.breakParagraph(boxes, 70.0, t)

    result shouldBe defined
    result.get.length shouldBe 2
    result.get(0).exists { case g: Glue => g.stretchOrder > 0; case _ => false } shouldBe true
  }

  "glue after a break is discarded — the next line never opens with a space" in {
    val t     = new HeadlessTypesetter
    val boxes = Seq(new W(30), Glue(4, 4, 1), new W(10), Glue(4, 4, 1), Glue(4, 4, 1), new W(40))

    // the break lands at the first of the two adjacent glues; the second must vanish with it
    val result = KnuthPlass.breakParagraph(boxes, 44.0, t)

    result shouldBe defined
    result.get.length shouldBe 2
    result.get(1).exists(_.isInstanceOf[Glue]) shouldBe false
    result.get(1).count(_.isInstanceOf[W]) shouldBe 1
  }

  "\\nobreak before a space forbids the break there" in {
    val t           = new HeadlessTypesetter
    def flex        = Glue(4, 4, 1)
    val without     = Seq(new W(30), flex, new W(10), flex, new W(40))
    val withNobreak = Seq(new W(30), flex, new W(10), new Penalty(Penalty.Inhibit), flex, new W(40))

    KnuthPlass.breakParagraph(without, 44.0, t) shouldBe defined
    // the same paragraph with \nobreak guarding its only feasible break has no legal solution
    KnuthPlass.breakParagraph(withNobreak, 44.0, t) shouldBe None
  }

  "a forcing penalty breaks the line even where it is underfull" in {
    val t     = new HeadlessTypesetter
    val boxes = Seq(new W(10), new Penalty(Penalty.Force), new W(10))

    val result = KnuthPlass.breakParagraph(boxes, 100.0, t)

    result shouldBe defined
    result.get.length shouldBe 2
  }

  "a broken discretionary's post material opens the following line" in {
    val t    = new HeadlessTypesetter
    val post = new W(9)
    val boxes = Seq(
      new W(10),
      Glue(2, 2, 0),
      new W(6),
      new DiscretionaryBox(Seq(new W(2)), Seq(post), Nil),
      new W(4),
    )

    // only the break at the discretionary fits: line 1 = 10+2+6+pre(2) = 20 exactly; line 2 opens with `post`
    val result = KnuthPlass.breakParagraph(boxes, 20.0, t)

    result shouldBe defined
    result.get.length shouldBe 2
    result.get(1).head should be theSameInstanceAs post
  }

  "\\adjdemerits steers away from an adjacent-line fitness jump" in {
    val t = new HeadlessTypesetter
    t.set("tolerance", 10000.0)

    // Two candidate breakings of the same two-line paragraph: ending line 1 after the second word gives a
    // loose line followed by a tight one (a two-class jump); carrying the third word gives a decent line
    // followed by the same tight last line. The jumpy chain has less badness, so with \adjdemerits 0 it
    // wins; a large \adjdemerits makes the jump expensive and flips the choice.
    def para = Seq(
      new W(50), Glue(4, 30, 0), new W(30), Glue(4, 2, 0), new W(2),
      Glue(4, 0, 12), new W(100), Glue(4, 0, 12).noBreak, new W(6),
    )

    t.set("adjdemerits", 0.0)
    KnuthPlass.breakParagraph(para, 100.0, t).get.head.count(_.isInstanceOf[W]) shouldBe 2

    t.set("adjdemerits", 1e6)
    KnuthPlass.breakParagraph(para, 100.0, t).get.head.count(_.isInstanceOf[W]) shouldBe 3
  }

  "z-fold panels restart at the sheet's top-left on the second sheet" in {
    class Probe extends ContentBox:
      var drawnAt: Option[(Double, Double)] = None
      val width: Double                     = 0
      val xAdvance: Double                  = 0
      val ascent: Double                    = 0
      val descent: Double                   = 0
      def draw(t: Typesetter, x: Double, y: Double): Unit = drawnAt = Some((x, y))

    val t = new HeadlessTypesetter
    t.set("layout", "zfold")
    val doc = new DocumentMode(t)

    for _ <- 0 until 6 do doc.add(new Probe) // fill the six panels of sheet one
    val seventh = new Probe
    doc.add(seventh) // the first panel of sheet two

    val (x, y) = seventh.drawnAt.get
    x shouldBe (t.getNumber("hoffset") +- 1e-9)
    y shouldBe (t.getNumber("voffset") +- 1e-9) // top-left again — not two panel-heights below the sheet
  }

  // A vertical galley that also records the line boxes the paragraph contributes to it.
  private class CapturingGalley(t0: Typesetter) extends VBoxBuilder(t0):
    val contributed = ArrayBuffer[Box]()
    override infix def add(box: Box): Unit =
      contributed += box
      super.add(box)

  "the greedy breaker discards every space at a line break, not just the first" in {
    val t = new HeadlessTypesetter
    t.set("hsize", 62.0)
    t.set("tolerance", 0.0) // no line justifies perfectly, so the greedy fallback runs

    val galley = new CapturingGalley(t)
    t.push(galley)
    val pm = new ParagraphMode(t)
    t.push(pm)

    pm add new CharBox(t, "wwwwwwwwww") // 60 — fills the first line
    pm add t.getGlue("spaceskip")
    pm add t.getGlue("spaceskip")
    pm add new CharBox(t, "dddd")
    pm.done()

    val lines = galley.contributed.collect { case h: HBox => h }
    lines.length shouldBe 2
    // the second interword space must not survive onto line two
    val spaceNat = t.getGlue("spaceskip").naturalSize
    lines(1).boxes.exists { case g: Glue => g.naturalSize == spaceNat; case _ => false } shouldBe false
  }

  "the bidi reorder never splits a surrogate pair" in {
    def wellFormed(s: String): Boolean =
      var i  = 0
      var ok = true
      while i < s.length do
        if Character.isHighSurrogate(s.charAt(i)) then
          if i + 1 >= s.length || !Character.isLowSurrogate(s.charAt(i + 1)) then ok = false
          i += 2
        else
          if Character.isLowSurrogate(s.charAt(i)) then ok = false
          i += 1
      ok

    val t = new HeadlessTypesetter
    t.set("hsize", 300.0)
    t.set("pardir", 1.0) // right-to-left base, so the reorder actually reverses runs

    val galley = new CapturingGalley(t)
    t.push(galley)
    val pm = new ParagraphMode(t)
    t.push(pm)

    pm add new CharBox(t, "שלום")
    pm add t.getGlue("spaceskip")
    pm add new CharBox(t, "😀") // an astral character (emoji) inside the RTL line
    pm add t.getGlue("spaceskip")
    pm add new CharBox(t, "עולם")
    pm.done()

    val texts = galley.contributed.collect { case h: HBox => h }.flatMap(_.boxes).collect { case c: CharBox => c.text }
    texts should not be empty
    all(texts.map(wellFormed)) shouldBe true
  }

  "an empty text box before a space does not crash the sentence-space rule" in {
    val t = new HeadlessTypesetter
    t.set("hsize", 300.0)
    t.push(new VBoxBuilder(t))
    val pm = new ParagraphMode(t)
    t.push(pm)

    noException should be thrownBy {
      pm add new CharBox(t, "")
      pm add new CharBox(t, " ")
      pm add new CharBox(t, "x")
      pm.done()
    }
  }
