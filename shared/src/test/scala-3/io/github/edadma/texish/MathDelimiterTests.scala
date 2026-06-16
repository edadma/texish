package io.github.edadma.texish

import io.github.edadma.texish.opentype.{GlyphAssembly, GlyphPart}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** Stage 5a: stretchy delimiters. On the fixed-metric [[HeadlessTypesetter]] (every glyph 6 wide, ascent 8,
  * descent 2, so height 10) with no MATH table, the assembly and centering geometry is exact and easy to
  * pin down; the real-font behaviour (picking a taller precomposed variant) is covered by the JVM/native
  * font tests.
  */
class MathDelimiterTests extends AnyFreeSpec with Matchers:

  def base(t: HeadlessTypesetter): MathFont = new MathFont(t, t.currentFont, None)

  private def part(glyph: Int, extender: Boolean) = GlyphPart(glyph, 0.0, 0.0, 0.0, if extender then 1 else 0)

  "an axis-centered box places its content's centre on the math axis" in {
    val t     = new HeadlessTypesetter
    val inner = base(t).glyphBox('x'.toInt) // ascent 8, descent 2, height 10
    val ax    = 3.0
    val box   = new AxisCenteredBox(inner, ax)

    box.ascent shouldBe (10.0 / 2 + ax)  // half the height above the axis
    box.descent shouldBe (10.0 / 2 - ax) // half below it
    box.width shouldBe inner.width
  }

  "an assembly stacks its parts, repeating the extender to span the target" in {
    val t   = new HeadlessTypesetter
    val asm = GlyphAssembly(0.0, Vector(part(100, false), part(101, true), part(102, false)))

    // each part is 10 tall, joints overlap by 2, so N parts span 10N - 2(N-1) = 8N + 2 points
    val box = new GlyphAssemblyBox(t, t.currentFont, t.currentColor, asm, target = 45, overlap = 2)

    box.ascent shouldBe 50.0 // six parts (extender repeated four times): 8*6 + 2
    box.descent shouldBe 0.0
    box.width shouldBe 6.0
  }

  "an assembly with no extender is used at its fixed size" in {
    val t   = new HeadlessTypesetter
    val asm = GlyphAssembly(0.0, Vector(part(100, false), part(102, false)))
    val box = new GlyphAssemblyBox(t, t.currentFont, t.currentColor, asm, target = 100, overlap = 2)

    box.ascent shouldBe 18.0 // two parts, one joint: 10 + 10 - 2
  }

  "an assembly draws its parts bottom to top" in {
    val rec = new RecordingGlyphTypesetter
    val asm = GlyphAssembly(0.0, Vector(part(100, false), part(101, true), part(102, false)))
    val box = new GlyphAssemblyBox(rec, rec.currentFont, rec.currentColor, asm, target = 20, overlap = 2)

    box.draw(rec, 0.0, 100.0)

    val bottomY = rec.drawn.collectFirst { case (g, _, y) if g == 100 => y }.get
    val topY    = rec.drawn.collectFirst { case (g, _, y) if g == 102 => y }.get
    bottomY should be > topY // the bottom cap sits lower on the page (larger y) than the top cap
  }

  "a delimited sub-formula flanks the inner box with two fences as an Inner atom" in {
    val t     = new HeadlessTypesetter
    val bf    = base(t)
    val m     = new MathMode(t, bf, MathStyle.Text)
    val inner = bf.glyphBox('x'.toInt)

    val box = m.makeDelimited(Some(0x28), inner, Some(0x29)).asInstanceOf[HBox]

    box.boxes should have length 3
    box.boxes.head shouldBe a[AxisCenteredBox]
    box.boxes.last shouldBe a[AxisCenteredBox]
  }

  "a null delimiter draws no fence on that side" in {
    val t     = new HeadlessTypesetter
    val bf    = base(t)
    val m     = new MathMode(t, bf, MathStyle.Text)
    val inner = bf.glyphBox('x'.toInt)

    val box = m.makeDelimited(None, inner, Some(0x29)).asInstanceOf[HBox]

    box.boxes should have length 2          // inner plus the single right fence
    box.boxes.last shouldBe a[AxisCenteredBox]
  }
