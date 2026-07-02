package io.github.edadma.texish

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** Stage 5b: math accents. On the fixed-metric [[HeadlessTypesetter]] the accent's exact height depends on
  * font-specific attachment data the stub lacks, so these pin the structural behaviour — an accent keeps the
  * nucleus's width, sets a single accent glyph over it, and enters as an Ord atom. The real-font vertical
  * placement (the accent rising above the nucleus) is covered by the JVM/native font tests.
  */
class MathAccentTests extends AnyFreeSpec with Matchers:

  def base(t: HeadlessTypesetter): MathFont = new MathFont(t, t.currentFont, None)

  def part(t: HeadlessTypesetter, bf: MathFont, c: Char): Box =
    val sm = new MathMode(t, bf, MathStyle.Text.cramp)
    sm.addChar(c)
    sm.result.asInstanceOf[Box]

  "an accent keeps the nucleus's width and is at least as tall" in {
    val t       = new HeadlessTypesetter
    val bf      = base(t)
    val m       = new MathMode(t, bf, MathStyle.Text)
    val nucleus = part(t, bf, 'x')

    val box = m.makeAccent(0x0302, nucleus, wide = false).asInstanceOf[AccentBox]

    box.width shouldBe nucleus.width
    box.ascent should be >= nucleus.ascent
  }

  "an accent draws one accent glyph over the nucleus" in {
    val rec = new RecordingGlyphTypesetter
    val bf  = base(rec)
    val m   = new MathMode(rec, bf, MathStyle.Text)
    val nucleus = { val s = new MathMode(rec, bf, MathStyle.Text.cramp); s.addChar('x'); s.result.asInstanceOf[Box] }

    rec.draw(m.makeAccent(0x0302, nucleus, wide = false))

    rec.drawn.count { case (g, _, _) => g == 0x0302 } shouldBe 1 // exactly one accent mark
    rec.drawn.exists { case (g, _, _) => g == 0x1D465 } shouldBe true // and the math-italic x nucleus
  }

  "an accent never sinks below its design height over a short nucleus" in {
    // AccentBaseHeight is the tallest base that needs no raising — a base *shorter* than it must leave the
    // accent at its design height, not pull it down into the nucleus. An empty box (ascent 0) is the
    // extreme case: the accent draws exactly at the baseline-relative position the font designed.
    val rec = new RecordingGlyphTypesetter
    val bf  = base(rec)
    val m   = new MathMode(rec, bf, MathStyle.Text)

    m.makeAccent(0x0302, HBox(Vector.empty), wide = false).draw(rec, 0, 0) // baseline at y = 0

    val accentY = rec.drawn.collectFirst { case (g, _, y) if g == 0x0302 => y }.get
    accentY shouldBe (0.0 +- 0.001) // unshifted — not lowered by the (negative) base excess
  }

  "an accented atom enters the list as an Ord atom" in {
    val t       = new HeadlessTypesetter
    val bf      = base(t)
    val m       = new MathMode(t, bf, MathStyle.Text)
    val nucleus = part(t, bf, 'x')

    m.addNode(MathAtom(MathClass.Ord, m.makeAccent(0x0302, nucleus, wide = false)))
    val box = m.result.asInstanceOf[HBox]

    box.boxes should have length 1
    box.boxes.head shouldBe a[AccentBox]
  }
