package io.github.edadma.typesetter

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** Stage 3 of math support: math styles and super/subscripts. These run on the fixed-metric
  * [[StubTypesetter]] (every glyph 6 wide, ascent 8, descent 2, regardless of size), so script geometry is
  * exact and font-independent. With no MATH table the math font falls back to TeX's Computer Modern script
  * parameters ([[MathScriptParams.texDefaults]]); the default text font is 14pt, so one em is 14.
  */
class MathScriptTests extends AnyFreeSpec with Matchers:

  def base(t: StubTypesetter): MathFont = new MathFont(t, t.currentFont, None)

  /** A script box laid out by a nested math mode at the given style, the way the parser builds one. */
  def script(t: StubTypesetter, bf: MathFont, style: MathStyle, c: Char): Box =
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
    val t  = new StubTypesetter
    val bf = base(t)

    // a script of a script is scriptscript: half the base (50%), not 70% of 70%
    new MathMode(t, bf, MathStyle.Text.sup).mathFont.size shouldBe (14 * 0.7 +- 0.001)
    new MathMode(t, bf, MathStyle.Text.sup.sup).mathFont.size shouldBe (14 * 0.5 +- 0.001)
    new MathMode(t, bf, MathStyle.Text).mathFont.size shouldBe (14.0 +- 0.001)
  }

  "a superscript is raised above the nucleus and shifts the box's ascent up" in {
    val t  = new StubTypesetter
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
    val t  = new StubTypesetter
    val bf = base(t)
    val m  = new MathMode(t, bf, MathStyle.Text)

    m.addChar('x')
    m.addScript(superscript = false, script(t, bf, MathStyle.Text.sub, '0'))
    val sb = m.result.asInstanceOf[HBox].boxes.head.asInstanceOf[MathScriptBox]

    sb.descent should be > 2.0    // the subscript drops below the nucleus's own depth
    sb.ascent shouldBe (8.0 +- 0.01)
  }

  "an atom with both scripts keeps them clear of each other" in {
    val t  = new StubTypesetter
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
    val t  = new StubTypesetter
    val bf = base(t)
    val m  = new MathMode(t, bf, MathStyle.Text)

    m.addScript(superscript = true, script(t, bf, MathStyle.Text.sup, '2'))
    val box = m.result.asInstanceOf[HBox]

    box.boxes should have length 1
    box.boxes.head shouldBe a[MathScriptBox]
  }

  "a second script of the same kind on one atom is an error" in {
    val t  = new StubTypesetter
    val bf = base(t)
    val m  = new MathMode(t, bf, MathStyle.Text)

    m.addChar('x')
    m.addScript(superscript = true, script(t, bf, MathStyle.Text.sup, '2'))
    an[Exception] should be thrownBy m.addScript(superscript = true, script(t, bf, MathStyle.Text.sup, '3'))
  }
