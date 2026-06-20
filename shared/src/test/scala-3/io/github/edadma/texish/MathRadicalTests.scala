package io.github.edadma.texish

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** Stage 4: radicals. On the fixed-metric [[HeadlessTypesetter]] (glyph 6 wide, ascent 8, descent 2) with no
  * MATH table, the surd is the base √ glyph and radical layout uses TeX's Computer Modern defaults
  * ([[RadicalParams.texDefaults]]) at the 14pt text font.
  */
class MathRadicalTests extends AnyFreeSpec with Matchers:

  def base(t: HeadlessTypesetter): MathFont = new MathFont(t, t.currentFont, None)

  def part(t: HeadlessTypesetter, bf: MathFont, style: MathStyle, c: Char): Box =
    val sm = new MathMode(t, bf, style)
    sm.addChar(c)
    sm.result.asInstanceOf[Box]

  "a radical is as wide as surd plus radicand plus the vinculum's overshoot, with the bar above the radicand" in {
    val t  = new HeadlessTypesetter
    val bf = base(t)
    val m  = new MathMode(t, bf, MathStyle.Text)
    val r  = part(t, bf, MathStyle.Text.cramp, 'x')

    val rad = m.makeRadical(r).asInstanceOf[RadicalBox]

    // surd 6 + radicand 6 + the overbar's overshoot past the radicand (0.05 × 14pt), so a glyph whose ink fills
    // its advance does not end flush with the bar.
    rad.width shouldBe (12.7 +- 0.001)
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

  "a degree (a cube root's index) widens the radical and rides high to the surd's left" in {
    val rec = new RecordingGlyphTypesetter
    val bf  = base(rec)
    val m   = new MathMode(rec, bf, MathStyle.Text)
    val r   = part(rec, bf, MathStyle.Text.cramp, '8')
    val deg = part(rec, bf, MathStyle.Text.rootDegree, '3')

    val plain  = m.makeRadical(r).asInstanceOf[RadicalBox]
    val rooted = m.makeRadical(r, Some(deg)).asInstanceOf[RadicalBox]

    rooted.width should be > plain.width   // the degree adds width on the left
    rooted.ascent should be >= plain.ascent

    rec.draw(rooted, 0.0, 100.0)
    val (_, degX, degY)   = rec.drawn.find { case (g, _, _) => g == '3'.toInt }.get
    val (_, randX, randY) = rec.drawn.find { case (g, _, _) => g == '8'.toInt }.get
    degX should be < randX // the index sits left of the radicand, in the surd's kink
    degY should be < randY // and rides high — its baseline is above the radicand's
  }

  "an oversized surd's surplus is centred, halving how far its point dips below the radicand" in {
    // The smallest precomposed surd a font offers is often far taller than a short radicand (a lone π) needs.
    // Hung from the bar by its top, all that surplus would dive below the baseline; TeX centres it instead, so
    // half raises the bar's clearance and half remains as the dip. A tall surd over a short, depthless radicand
    // exercises that: with RuleBoxes standing in for the glyphs, only the box metrics matter here.
    val t        = new HeadlessTypesetter
    val p        = RadicalParams.texDefaults(14, display = false)
    val surd     = new RuleBox(t, 6, 1.0, 19.0) // height 20, mostly hanging below its own baseline
    val radicand = new RuleBox(t, 6, 5.0, 0.0)  // height 5, no depth — the surd is much taller than needed
    val rad      = new RadicalBox(t, surd, radicand, p)

    val minBarTop = radicand.ascent + p.verticalGap + p.ruleThickness
    val surplus   = surd.height - minBarTop - radicand.descent // would all dive below without centring

    rad.descent shouldBe (radicand.descent + surplus / 2 +- 0.001) // half the surplus, not all of it
    rad.descent should be < surplus                               // strictly shallower than the un-centred dip
  }

  "a radical enters the list as an Ord atom and lays out with positive size" in {
    val t  = new HeadlessTypesetter
    val bf = base(t)
    val m  = new MathMode(t, bf, MathStyle.Text)
    val r  = part(t, bf, MathStyle.Text.cramp, 'x')

    m.addNode(MathAtom(MathClass.Ord, m.makeRadical(r)))
    val box = m.result.asInstanceOf[HBox]

    box.boxes should have length 1
    box.boxes.head shouldBe a[RadicalBox]
    box.height should be > 0.0
  }
