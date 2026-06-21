package io.github.edadma.texish

/** How leaders distribute their copies across the space the glue is set to. `Aligned` (TeX's `\leaders`) tiles on
  * a fixed grid anchored at the page origin, so leaders on adjacent lines line up vertically; `Centered`
  * (`\cleaders`) centres a whole number of copies in the span; `Expanded` (`\xleaders`) spreads the leftover
  * space evenly between the copies. */
enum LeaderKind:
  case Aligned, Centered, Expanded

/** Flexible space that, once the line is set, is filled by repeating a unit box rather than left blank — TeX's
  * leaders. It behaves exactly as its glue while the line is broken and set (so a `\hfil`-stretch leader absorbs
  * all the slack), then [[ListBoxBuilder]] resolves it to a [[LeaderBox]] of the chosen width. The classic use is
  * a table of contents: `text \dotfill page` fills the gap between an entry and its page number with dots. */
class LeaderGlue(
    val unit: Box,
    val kind: LeaderKind,
    naturalSize: Double,
    stretch: Double = 0,
    shrink: Double = 0,
    stretchOrder: Int = 0,
    shrinkOrder: Int = 0,
) extends Glue(naturalSize, stretch, shrink, stretchOrder, shrinkOrder):
  /** The concrete box this leader becomes once the glue has been set to `width`. */
  def setTo(width: Double): Box = new LeaderBox(width, unit, kind)

  override def toString: String = s"LeaderGlue($kind, unit=$unit, naturalSize=$naturalSize)"

/** A resolved leader: `width` of horizontal space filled with copies of `unit`. A rule unit fills continuously
  * (the rule is simply re-widthed); a box unit is tiled according to `kind`. The box takes its vertical extent
  * from the unit so a leader on a line of its own still has height. */
class LeaderBox(val width: Double, val unit: Box, val kind: LeaderKind) extends ContentBox:
  val ascent: Double   = unit.ascent
  val descent: Double  = unit.descent
  val xAdvance: Double = width

  def draw(t: Typesetter, x: Double, y: Double): Unit =
    box(t, x, y, "lightgreen")
    unit match
      case r: RuleBox =>
        // a rule leader (\hrulefill) draws as one continuous rule spanning the whole space
        r.withWidth(width).draw(t, x, y)
      case _ =>
        val w = unit.width
        if w > 0 && width >= w then
          kind match
            case LeaderKind.Aligned =>
              // tile on a grid anchored at x=0, so leaders on neighbouring lines align
              var cx = math.ceil(x / w) * w
              while cx + w <= x + width + 1e-6 do
                unit.draw(t, cx, y)
                cx += w
            case LeaderKind.Centered =>
              val n   = math.floor(width / w).toInt
              var cx  = x + (width - n * w) / 2
              for _ <- 0 until n do
                unit.draw(t, cx, y)
                cx += w
            case LeaderKind.Expanded =>
              val n   = math.floor(width / w).toInt
              val gap = if n > 0 then (width - n * w) / (n + 1) else 0.0
              var cx  = x + gap
              for _ <- 0 until n do
                unit.draw(t, cx, y)
                cx += w + gap

  override def toString: String = s"LeaderBox(width=$width, $kind, unit=$unit)"
