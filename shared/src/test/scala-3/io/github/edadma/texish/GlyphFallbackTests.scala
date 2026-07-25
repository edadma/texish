package io.github.edadma.texish

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** Glyph fallback: a text run is split so codepoints the active font has no glyph for are set from a fallback face
  * instead of drawing a missing-glyph box. Modelled here with a primary face that lacks every codepoint at or above
  * U+0100 (as Latin Modern lacks Greek) and a fallback face that has them all. Non-ASCII characters are built from
  * their codepoints to keep the source ASCII. */
class GlyphFallbackTests extends AnyFreeSpec with Matchers:

  private def cp(n: Int): String = new String(Character.toChars(n))
  private val alpha = cp(0x03B1) // Greek small alpha — fallback-only (>= U+0100)
  private val beta  = cp(0x03B2) // Greek small beta
  private val gamma = cp(0x03B3) // Greek small gamma
  private val pua   = cp(0xE000) // a private-use codepoint — in neither font in the last test

  // Every cut of the primary face lacks the high codepoints, so a styled run falls back exactly as a plain one does.
  private class T extends HeadlessTypesetter:
    override def glyphIndex(font: RenderFont, c: Int): Int =
      if font.startsWith("primaryface") && c >= 0x100 then 0 else c

  private def setup(): (T, Font) =
    val t = new T
    t.loadFont("primary", "primaryface", Set.empty[String], Set.empty[String])
    t.loadFont("fallback", "fallbackface", Set.empty[String], Set.empty[String])
    t.fallbackTypeface = Some("fallback")
    (t, t.makeFont("primary", 10, Set.empty))

  "coverageSegments keeps a fully-covered run as one segment in the primary font" in {
    val (t, primary) = setup()
    val segs = t.coverageSegments("hello", primary).toSeq
    segs.map(_._1) shouldBe Seq("hello")
    segs.map(_._2.typeface) shouldBe Seq("primary")
  }

  "coverageSegments splits an uncovered codepoint out into the fallback font" in {
    val (t, primary) = setup()
    val segs = t.coverageSegments("a" + alpha + "b", primary).toSeq
    segs.map(_._1) shouldBe Seq("a", alpha, "b")
    segs.map(_._2.typeface) shouldBe Seq("primary", "fallback", "primary")
  }

  "coverageSegments groups consecutive uncovered codepoints into one fallback segment" in {
    val (t, primary) = setup()
    val segs = t.coverageSegments(alpha + beta + gamma + "x", primary).toSeq
    segs.map(_._1) shouldBe Seq(alpha + beta + gamma, "x")
    segs.map(_._2.typeface) shouldBe Seq("fallback", "primary")
  }

  "coverageSegments leaves the run whole when no fallback face is registered" in {
    val (t, primary) = setup()
    t.fallbackTypeface = None
    t.coverageSegments("a" + alpha + "b", primary).toSeq.map(_._1) shouldBe Seq("a" + alpha + "b")
  }

  // The substituted glyphs must match the weight and slope of the text around them: a Cyrillic word inside a bold
  // heading is set from the fallback's bold cut, so the weight does not break mid-phrase.

  private def styled(): T =
    val t = new T
    t.loadFont("primary", "primaryface", Set.empty[String], Set.empty[String])
    t.loadFont("primary", "primaryface-bold", Set.empty[String], Set("bold"))
    t.loadFont("primary", "primaryface-mono", Set.empty[String], Set("mono"))
    t.loadFont("fallback", "fallbackface", Set.empty[String], Set.empty[String])
    t.loadFont("fallback", "fallbackface-bold", Set.empty[String], Set("bold"))
    t.fallbackTypeface = Some("fallback")
    t

  "a fallback segment takes the primary's weight, not the regular cut" in {
    val t    = styled()
    val segs = t.coverageSegments("a" + alpha + "b", t.makeFont("primary", 10, Set("bold"))).toSeq
    segs.map(_._1) shouldBe Seq("a", alpha, "b")
    // The real proof is which face file each segment resolved to — the fallback picked its *bold* cut.
    segs.map(_._2.renderFont) shouldBe Seq("primaryface-bold", "fallbackface-bold", "primaryface-bold")
    segs.map(_._2.style) shouldBe Seq(Set("bold"), Set("bold"), Set("bold"))
  }

  "a fallback with no cut for the requested style degrades to what it has" in {
    // The fallback here has no italic cut; the segment must still set (in the regular cut) rather than fail.
    val t    = styled()
    val segs = t.coverageSegments(alpha, t.makeFont("primary", 10, Set("italic"))).toSeq
    segs.map(_._2.typeface) shouldBe Seq("fallback")
    segs.map(_._2.renderFont) shouldBe Seq("fallbackface")
  }

  "the mono/sans role is dropped rather than redirecting the fallback to the typewriter family" in {
    // The fallback stands in for coverage, not family. Keeping the role would send makeFont to the mono default
    // (an unrelated family, no likelier to have the glyph — and not loaded here at all).
    val t    = styled()
    val segs = t.coverageSegments("a" + alpha, t.makeFont("primary", 10, Set("mono"))).toSeq
    segs.map(_._2.typeface) shouldBe Seq("primary", "fallback")
    segs.map(_._2.renderFont) shouldBe Seq("primaryface-mono", "fallbackface")
  }

  "a codepoint neither font has stays with the primary" in {
    val t = new HeadlessTypesetter:
      override def glyphIndex(font: RenderFont, c: Int): Int =
        if c == 0xE000 then 0 else if font == "primaryface" && c >= 0x100 then 0 else c
    t.loadFont("primary", "primaryface", Set.empty[String], Set.empty[String])
    t.loadFont("fallback", "fallbackface", Set.empty[String], Set.empty[String])
    t.fallbackTypeface = Some("fallback")
    val primary = t.makeFont("primary", 10, Set.empty)
    val segs    = t.coverageSegments(pua + alpha, primary).toSeq
    segs.map(_._1) shouldBe Seq(pua, alpha)
    segs.map(_._2.typeface) shouldBe Seq("primary", "fallback")
  }
