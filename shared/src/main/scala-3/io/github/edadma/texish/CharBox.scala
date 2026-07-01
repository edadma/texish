package io.github.edadma.texish

import io.github.edadma.texish.opentype.{GlyphPlacement, Gpos, JoiningForm}

/** A run of text set in one font and colour. Latin text takes the plain path: the backend measures and
  * draws the string directly. Several scripts need the engine to place glyphs itself, and all end at the
  * same positioned-glyph drawing.
  *
  * Hebrew niqqud are combining marks: the letters carry their own advances, and each mark is positioned by
  * a font anchor relative to its base (see the GPOS shaper).
  *
  * Arabic letters take a contextual shape — initial, medial, final or isolated — chosen from their
  * neighbours. When this box carries the resolved `JoiningForm` of each character (decided in memory
  * order by the paragraph reorderer) and the font shapes Arabic, the box runs the characters through the
  * GSUB shaper: composition substitutions may split a dotted letter into a dotless skeleton plus a separate
  * dot, then the skeleton takes its connecting form.
  *
  * Indic scripts (Devanagari, Bengali, …) are left to right, so they carry no pre-resolved forms; the box
  * detects the script from the text and, when the font shapes it, runs the Indic shaper — clustering, conjunct
  * and half-form substitution, the pre-base vowel-sign reordering, and vowel-sign variant selection.
  *
  * In every case the resulting glyphs are laid out by their own advances, and any marks (Hebrew points,
  * Arabic dots, Indic below-base signs) are placed by the shared GPOS mark shaper. */
class CharBox(t: Typesetter, val text: String, val font: Font, val color: Color, val forms: Array[JoiningForm])
    extends ContentBox:
  def this(t: Typesetter, text: String, font: Font, color: Color) = this(t, text, font, color, null)
  def this(t: Typesetter, text: String) = this(t, text, t.currentFont, t.currentColor, null)

  private val rf = font.renderFont.asInstanceOf[t.RenderFont]

  // The shaped glyph run — the glyphs to draw after the script's substitutions — when the box needs the
  // engine to place glyphs: an Arabic run whose joining forms the paragraph reorderer resolved, or a
  // Devanagari run detected from the text. Otherwise null, and the box draws as ordinary text.
  private val shapedGlyphs: Array[Int] =
    if text.isEmpty then null
    else if forms != null then
      t.gsubShaper(rf) match
        case Some(gs) =>
          val nominal = Array.tabulate(text.length)(i => t.glyphIndex(rf, text.charAt(i).toInt))
          gs.shape(nominal, forms)
        case None => null
    else
      t.indicShaper(rf) match
        case Some(sh) if sh.handles(text) =>
          val cps = Array.tabulate(text.length)(i => text.charAt(i).toInt)
          sh.shape(cps, cp => t.glyphIndex(rf, cp))
        case _ => null

  // Placement of each shaped glyph: where the marks attach to their bases (GPOS), or a run of bare advances
  // when the font has no mark positioning. Computed once for both metrics and drawing.
  private val shapedPlaces: Array[GlyphPlacement] =
    if shapedGlyphs == null then null
    else
      t.markShaper(rf) match
        case Some(sh) => sh.position(shapedGlyphs)
        case None     => Array.fill(shapedGlyphs.length)(GlyphPlacement(false, -1, 0.0, 0.0))

  val TextExtents(_, yBearing, _, heightValue, advance, _) = t.getTextExtents(text, rf)

  // The box width is the pen advance, not the ink bounding box. The advance carries the glyphs' side
  // bearings, so one box sets flush after the next exactly as the font intends — and manual \kern between
  // pieces of text (as in the \TeX logo) lands on the same metrics it would in TeX. Using the ink width
  // instead silently dropped the leading and trailing side bearings of every run; justification mostly hid
  // it, but exact character spacing did not survive. The ink box still gives the vertical extents below.
  // A shaped run measures by the sum of its base glyphs' advances; the marks add nothing.
  override val width: Double =
    if shapedGlyphs == null then advance
    else
      var w = 0.0
      var i = 0
      while i < shapedGlyphs.length do
        if !shapedPlaces(i).isMark then w += t.glyphExtents(rf, shapedGlyphs(i)).xAdvance
        i += 1
      w
  override val xAdvance: Double = width
  override val height: Double   = heightValue
  val ascent: Double            = -yBearing       // Ascent is the negative yBearing
  val descent: Double           = height - ascent // Descent is height minus ascent

  def draw(t: Typesetter, x: Double, y: Double): Unit =
    box(t, x, y, "purple")

    if text.nonEmpty then
      t.setFont(font)
      t.setColor(color)
      val rf = font.renderFont.asInstanceOf[t.RenderFont]
      if shapedGlyphs != null then drawPositioned(shapedGlyphs, shapedPlaces, x, y)
      else
        t.markShaper(rf) match
          case Some(shaper) if Bidi.hasMarks(text) => drawMarked(t, rf, shaper, x, y)
          case _                                   => t.drawString(text, x, y)

  /** Draw a positioned glyph run: a base advances the pen and is drawn at the cursor; a mark advances
    * nothing and is drawn relative to the origin of the glyph it attaches to, so a vowel point or a
    * decomposed letter-dot sits under, over or inside its base. The glyphs are already in visual order, so
    * a left-to-right pass is correct. */
  private def drawPositioned(glyphs: Array[Int], places: Array[GlyphPlacement], x: Double, y: Double): Unit =
    val originX = new Array[Double](glyphs.length)
    val originY = new Array[Double](glyphs.length)
    var cursor  = x
    var i       = 0
    while i < glyphs.length do
      val p = places(i)
      val (ox, oy) =
        if p.attach >= 0 then (originX(p.attach) + p.dxEm * font.size, originY(p.attach) - p.dyEm * font.size)
        else (cursor, y)
      t.drawGlyph(rf, glyphs(i), ox, oy)
      originX(i) = ox
      originY(i) = oy
      if !p.isMark then cursor += t.glyphExtents(rf, glyphs(i)).xAdvance
      i += 1

  /** Draw a run of Hebrew text with combining marks: map the characters to glyphs, position the marks by
    * the GPOS shaper, and hand off to the positioned-glyph drawing. */
  private def drawMarked(t: Typesetter, rf: t.RenderFont, shaper: Gpos, x: Double, y: Double): Unit =
    val cpBuf = scala.collection.mutable.ArrayBuffer.empty[Int]
    var ci    = 0
    while ci < text.length do
      val cp = text.codePointAt(ci)
      cpBuf += cp
      ci += Character.charCount(cp)
    val glyphs = cpBuf.toArray.map(cp => t.glyphIndex(rf, cp))
    drawPositioned(glyphs, shaper.position(glyphs), x, y)

  def newCharBox(s: String): CharBox = new CharBox(t, s, font, color)

  /** A sibling box of this run's font and colour carrying per-character joining forms — used by the paragraph
    * reorderer to hand a visual-order Arabic run the forms it resolved in memory order. */
  def newCharBox(s: String, forms: Array[JoiningForm]): CharBox = new CharBox(t, s, font, color, forms)

  override def toString: String =
    s"CharBox(ascent=$ascent, descent=$descent, width=$width, typeface=${font.typeface}, \"$text\")"
