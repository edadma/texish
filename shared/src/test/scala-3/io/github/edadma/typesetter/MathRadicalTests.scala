package io.github.edadma.typesetter

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** Stage 4: radicals. On the fixed-metric [[StubTypesetter]] (glyph 6 wide, ascent 8, descent 2) with no
  * MATH table, the surd is the base √ glyph and radical layout uses TeX's Computer Modern defaults
  * ([[RadicalParams.texDefaults]]) at the 14pt text font.
  */
class MathRadicalTests extends AnyFreeSpec with Matchers:

  def base(t: StubTypesetter): MathFont = new MathFont(t, t.currentFont, None)

  def part(t: StubTypesetter, bf: MathFont, style: MathStyle, c: Char): Box =
    val sm = new MathMode(t, bf, style)
    sm.addChar(c)
    sm.result.asInstanceOf[Box]

  "a radical sets the bar above the radicand and is as wide as surd plus radicand" in {
    val t  = new StubTypesetter
    val bf = base(t)
    val m  = new MathMode(t, bf, MathStyle.Text)
    val r  = part(t, bf, MathStyle.Text.cramp, 'x')

    val rad = m.makeRadical(r).asInstanceOf[RadicalBox]

    rad.width shouldBe (12.0 +- 0.001)  // surd 6 + radicand 6
    rad.ascent should be > r.ascent     // the vinculum and its clearance rise above the radicand top (8)
    rad.descent should be >= r.descent  // at least as deep as the radicand
  }

  "the surd draws to the left of the radicand" in {
    val rec = new RecordingGlyphTypesetter
    val bf  = base(rec)
    val m   = new MathMode(rec, bf, MathStyle.Text)
    val r   = part(rec, bf, MathStyle.Text.cramp, '9')

    rec.draw(m.makeRadical(r))

    val surdX = rec.drawn.collectFirst { case (g, x, _) if g == 0x221A => x }.get
    val randX = rec.drawn.collectFirst { case (g, x, _) if g == '9'.toInt => x }.get
    surdX shouldBe (0.0 +- 0.001)
    randX should be > surdX // the radicand sits to the right of the surd
  }

  "a radical enters the list as an Ord atom and lays out with positive size" in {
    val t  = new StubTypesetter
    val bf = base(t)
    val m  = new MathMode(t, bf, MathStyle.Text)
    val r  = part(t, bf, MathStyle.Text.cramp, 'x')

    m.addNode(MathAtom(MathClass.Ord, m.makeRadical(r)))
    val box = m.result.asInstanceOf[HBox]

    box.boxes should have length 1
    box.boxes.head shouldBe a[RadicalBox]
    box.height should be > 0.0
  }
