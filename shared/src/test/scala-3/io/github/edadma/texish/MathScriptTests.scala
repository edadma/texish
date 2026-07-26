package io.github.edadma.texish

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** Stage 3 of math support: math styles and super/subscripts. These run on the fixed-metric
  * [[HeadlessTypesetter]] (every glyph 6 wide, ascent 8, descent 2, regardless of size), so script geometry is
  * exact and font-independent. With no MATH table the math font falls back to TeX's Computer Modern script
  * parameters ([[MathScriptParams.texDefaults]]); the default text font is 14pt, so one em is 14.
  */
class MathScriptTests extends AnyFreeSpec with Matchers:

  /** The math font these expectations are derived from. The size is pinned rather than inherited, so the
    * geometry below stays meaningful whatever the engine's default text size happens to be. */
  def base(t: HeadlessTypesetter): MathFont =
    t.selectFont("lmroman", 14, Set("regular"))
    new MathFont(t, t.currentFont, None)

  /** A script box laid out by a nested math mode at the given style, the way the parser builds one. */
  def script(t: HeadlessTypesetter, bf: MathFont, style: MathStyle, c: Char): Box =
    val sm = new MathMode(t, bf, style)
    sm.addChar(c)
    sm.result.asInstanceOf[Box]

  "the style transitions follow TeX: script is smaller, subscripts cramp" in {
    import MathSize.*

    MathStyle.Text.sup shouldBe MathStyle(Script, cramped = false)
    MathStyle.Text.sub shouldBe MathStyle(Script, cramped = true)
    MathStyle(Script, cramped = false).sup shouldBe MathStyle(ScriptScript, cramped = false)
    MathStyle(ScriptScript, cramped = true).sub shouldBe MathStyle(ScriptScript, cramped = true)
    MathStyle.Text.cramp shouldBe MathStyle(Text, cramped = true)
  }

  "script sizes are absolute fractions of the base, not compounded" in {
    val t  = new HeadlessTypesetter
    val bf = base(t)

    // a script of a script is scriptscript: half the base (50%), not 70% of 70%
    new MathMode(t, bf, MathStyle.Text.sup).mathFont.size shouldBe (14 * 0.7 +- 0.001)
    new MathMode(t, bf, MathStyle.Text.sup.sup).mathFont.size shouldBe (14 * 0.5 +- 0.001)
    new MathMode(t, bf, MathStyle.Text).mathFont.size shouldBe (14.0 +- 0.001)
  }

  "a superscript is raised above the nucleus and shifts the box's ascent up" in {
    val t  = new HeadlessTypesetter
    val bf = base(t)
    val m  = new MathMode(t, bf, MathStyle.Text)

    m.addChar('x')
    m.addScript(superscript = true, script(t, bf, MathStyle.Text.sup, '2'))
    val box = m.result.asInstanceOf[HBox]

    box.boxes should have length 1
    val sb = box.boxes.head.asInstanceOf[MathScriptBox]

    // ascent climbs from the nucleus's 8 to (superscriptShiftUp 0.362·14 = 5.068) + the script's ascent 8
    sb.ascent shouldBe (5.068 + 8 +- 0.01)
    sb.descent shouldBe (2.0 +- 0.01)        // no subscript — depth is the nucleus's
    sb.width should be > 12.0                // nucleus 6 + script 6 + a trailing script space
  }

  "a subscript is lowered below the nucleus and deepens the box" in {
    val t  = new HeadlessTypesetter
    val bf = base(t)
    val m  = new MathMode(t, bf, MathStyle.Text)

    m.addChar('x')
    m.addScript(superscript = false, script(t, bf, MathStyle.Text.sub, '0'))
    val sb = m.result.asInstanceOf[HBox].boxes.head.asInstanceOf[MathScriptBox]

    sb.descent should be > 2.0    // the subscript drops below the nucleus's own depth
    sb.ascent shouldBe (8.0 +- 0.01)
  }

  "an atom with both scripts keeps them clear of each other" in {
    val t  = new HeadlessTypesetter
    val bf = base(t)
    val m  = new MathMode(t, bf, MathStyle.Text)

    m.addChar('x')
    m.addScript(superscript = true, script(t, bf, MathStyle.Text.sup, '2'))
    m.addScript(superscript = false, script(t, bf, MathStyle.Text.sub, '0'))
    val sb = m.result.asInstanceOf[HBox].boxes.head.asInstanceOf[MathScriptBox]

    // both present: the box grows above and below the bare nucleus
    sb.ascent should be > 8.0
    sb.descent should be > 2.0
  }

  "a script with no preceding atom attaches to an empty nucleus" in {
    val t  = new HeadlessTypesetter
    val bf = base(t)
    val m  = new MathMode(t, bf, MathStyle.Text)

    m.addScript(superscript = true, script(t, bf, MathStyle.Text.sup, '2'))
    val box = m.result.asInstanceOf[HBox]

    box.boxes should have length 1
    box.boxes.head shouldBe a[MathScriptBox]
  }

  "a large operator skews its limits left; an ordinary atom sets the superscript out right" in {
    // both classes carry the same italic correction (5) on a 6-wide nucleus; only the class differs, so the
    // horizontal placement of the limits is what these assertions isolate
    def place(cls: MathClass): (Double, Double) =
      val rec = new RecordingGlyphTypesetter
      val bf  = new MathFont(rec, rec.currentFont, None)
      val m   = new MathMode(rec, bf, MathStyle.Text)

      // digit scripts stay upright (no math-italic remapping), so their glyph index is the digit itself
      m.addNode(MathAtom(cls, bf.glyphBox('I'.toInt), italicCorrection = 5.0))
      m.addScript(superscript = true, script(rec, bf, MathStyle.Text.sup, '7'))
      m.addScript(superscript = false, script(rec, bf, MathStyle.Text.sub, '3'))
      rec.draw(m.result.asInstanceOf[HBox])

      val subX = rec.drawn.collectFirst { case (g, x, _) if g == '3'.toInt => x }.get
      val supX = rec.drawn.collectFirst { case (g, x, _) if g == '7'.toInt => x }.get
      (subX, supX)

    val (opSub, opSup) = place(MathClass.Op)
    opSub shouldBe (1.0 +- 0.001) // 6 − 5: the subscript tucks left under the leaning operator
    opSup shouldBe (6.0 +- 0.001) // the superscript stays at the operator's right edge

    val (ordSub, ordSup) = place(MathClass.Ord)
    ordSub shouldBe (6.0 +- 0.001)  // an ordinary subscript sits at the nucleus's right edge
    ordSup shouldBe (11.0 +- 0.001) // 6 + 5: the superscript is set out past the slanted nucleus
  }

  "a glyph nucleus takes the standard script shifts; only a boxed nucleus tracks its own height" in {
    // rule 18a: a single character's scripts sit at the standard raise/drop, so x² and d² line up; a
    // built-up box (a fraction, a parenthesized group) pushes its scripts out past its own extent via the
    // baseline-drop constants. Params are chosen so the drop terms would dominate if they applied.
    val t     = new HeadlessTypesetter
    val bf    = base(t)
    val glyph = bf.glyphBox('d'.toInt)  // ascent 8, descent 2 on the stub
    val boxed = HBox(Vector(glyph))     // identical metrics, but a box, not a char
    val p = MathScriptParams(
      superscriptShiftUp                = 5.0,
      superscriptShiftUpCramped         = 4.0,
      superscriptBottomMin              = 0.0,
      superscriptBaselineDropMax        = 1.0, // boxed: ascent 8 − 1 = 7 > the standard 5
      subscriptShiftDown                = 2.0,
      subscriptTopMax                   = 100.0, // keep the script-top rule out of the way
      subscriptBaselineDropMin          = 1.0,   // boxed: descent 2 + 1 = 3 > the standard 2
      subSuperscriptGapMin              = 0.0,
      superscriptBottomMaxWithSubscript = 0.0,
      spaceAfterScript                  = 0.0,
    )
    val sup = Some(bf.glyphBox('2'.toInt))
    val sub = Some(bf.glyphBox('0'.toInt))

    MathScriptBox.shifts(glyph, sup, None, p, cramped = false)._1 shouldBe (5.0 +- 0.001)
    MathScriptBox.shifts(boxed, sup, None, p, cramped = false)._1 shouldBe (7.0 +- 0.001)
    MathScriptBox.shifts(glyph, None, sub, p, cramped = false)._2 shouldBe (2.0 +- 0.001)
    MathScriptBox.shifts(boxed, None, sub, p, cramped = false)._2 shouldBe (3.0 +- 0.001)
  }

  "a second script of the same kind on one atom is an error" in {
    val t  = new HeadlessTypesetter
    val bf = base(t)
    val m  = new MathMode(t, bf, MathStyle.Text)

    m.addChar('x')
    m.addScript(superscript = true, script(t, bf, MathStyle.Text.sup, '2'))
    an[Exception] should be thrownBy m.addScript(superscript = true, script(t, bf, MathStyle.Text.sup, '3'))
  }
