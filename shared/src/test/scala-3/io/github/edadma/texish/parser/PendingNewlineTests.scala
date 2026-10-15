package io.github.edadma.texish.parser

import io.github.edadma.texish.{Box, Builder, CharBox, HBox, HeadlessTypesetter, ParagraphMode, VerticalBox}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import java.io.ByteArrayOutputStream

// A lone newline between words is a deferred interword space. When the paragraph it belonged to is closed by
// something other than a blank line — a vertical command, or a group/environment ending — that deferred space
// is stale: the next paragraph must not open with it. A class check ("still horizontal") cannot tell, because
// the next paragraph is horizontal too; the handler tracks the paragraph the newline was seen in instead.
class PendingNewlineTests extends AnyFreeSpec with Matchers:

  // The x of the marker box (the width-18 \hbox each item opens with) on every set line. A leaked interword
  // space would sit before that box and push its x to the right on the offending line.
  private def markerXs(src: String): List[Double] =
    val t = new HeadlessTypesetter
    t.set("vsize", 1.0e9)
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)
    Console.withOut(new ByteArrayOutputStream) {
      proc.process(src)
      t.paragraph()
    }
    t.mode
      .asInstanceOf[Builder]
      .list
      .collect { case h: HBox => h }
      .flatMap { h =>
        var x = 0.0
        var found: Option[Double] = None
        h.boxes.foreach { b =>
          if found.isEmpty && b.isInstanceOf[HBox] && math.abs(b.width - 18.0) < 0.01 then found = Some(x)
          x += b.width
        }
        found
      }
      .toList

  "an item opening with a marker box keeps the same x on the first line after \\end" in {
    // `inside.` is followed by a single newline before \end, which defers an interword space; \end ends that
    // paragraph, so the marker box of the item after it must start at the same x as every other item, not be
    // pushed right by a leaked space.
    val xs = markerXs(
      """\newenvironment grp {} {}
\noindent\hbox to:18 {A\hfil}one.

\begin{grp}
\noindent\hbox to:18 {C\hfil}inside.
\end{grp}

\noindent\hbox to:18 {D\hfil}after end.

\noindent\hbox to:18 {E\hfil}second after.
""",
    )
    xs.length shouldBe 4
    xs.distinct shouldBe List(0.0) // every marker box opens its line at the same x
  }

  "a single newline between words is still an interword space within a paragraph" in {
    val t = new HeadlessTypesetter
    t.set("vsize", 1.0e9)
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)
    Console.withOut(new ByteArrayOutputStream) {
      proc.process("\\noindent hello\nworld.\n")
      t.paragraph()
    }
    val line = t.mode.asInstanceOf[Builder].list.collect { case h: HBox => h }.head
    // hello + space + world. — the space between the two words survives as interword glue (a positive-width
    // box between the two CharBox runs), so the line is wider than the two words set flush together.
    line.boxes.exists(b => b.isSpace) shouldBe true
  }

  // A line break at the very start of a paragraph is a leading space, and a leading space sets nothing — exactly
  // as a leading Space token is already dropped. Each case measures the x of the first glyph on the first line,
  // descending into nested boxes (a \parbox or minipage on the line), against the same text written flush.
  "a line break at the start of a paragraph sets no leading space" - {
    def lines(src: String): List[HBox] =
      val t = new HeadlessTypesetter
      t.set("vsize", 1.0e9)
      val handler = new TypesetterHandler(t)
      val proc    = new Processor(handler)
      registerTypesettingPrimitives(proc, handler)
      Console.withOut(new ByteArrayOutputStream) {
        proc.process(src)
        t.paragraph()
      }
      t.mode.asInstanceOf[Builder].list.collect { case h: HBox => h }.toList

    // The first text run inside `b` and its x relative to `b`'s left edge. The text is part of the answer because a
    // space set into an empty paragraph is a CharBox " " that the following word merges into (" Word"), so the
    // run's x alone cannot see it.
    def firstRun(b: Box): Option[(Double, String)] =
      b match
        case c: CharBox => Some((0.0, c.text))
        case h: HBox =>
          var x                               = 0.0
          var found: Option[(Double, String)] = None
          h.boxes.foreach { c =>
            if found.isEmpty then found = firstRun(c).map((cx, s) => (cx + x, s))
            x += c.width
          }
          found
        case v: VerticalBox => v.boxes.iterator.flatMap(firstRun).nextOption()
        case _              => None

    def lead(src: String): (Double, String) = firstRun(lines(src).head).get

    // The open paragraph's list up to and including its first text run, before any line breaking: what the
    // handler put there, independent of whether a line breaker would later discard glue at a line's start.
    def opening(src: String): List[(String, Double | String)] =
      val t = new HeadlessTypesetter
      t.set("vsize", 1.0e9)
      val handler = new TypesetterHandler(t)
      val proc    = new Processor(handler)
      registerTypesettingPrimitives(proc, handler)
      Console.withOut(new ByteArrayOutputStream)(proc.process(src))
      val list = t.mode.asInstanceOf[ParagraphMode].list
      val head = list.takeWhile(!_.isInstanceOf[CharBox]).map(b => (b.getClass.getSimpleName, b.width: Double | String))
      head ++ list.collectFirst { case c: CharBox => ("CharBox", c.text: Double | String) }

    "after \\noindent and a group" in {
      opening("\\noindent{\nWord and more.") shouldBe opening("\\noindent{Word and more.")
      lead("\\noindent{\nWord and more.}\n") shouldBe ((0.0, "Word"))
    }

    "in a macro argument set at the paragraph's start" in {
      val defs = "\\def same a {\\a}\n"
      opening(defs + "\\noindent\\same{\nWord and more.}") shouldBe opening(defs + "\\noindent\\same{Word and more.}")
      lead(defs + "\\noindent\\same{\nWord and more.}\n") shouldBe ((0.0, "Word"))
    }

    "after \\indent the indent stays and no space is added" in {
      opening("\\indent{\nWord and more.") shouldBe opening("\\indent{Word and more.")
      val flush = lead("\\indent{Word and more.}\n")
      flush._1 should be > 0.0
      lead("\\indent{\nWord and more.}\n") shouldBe flush
    }

    "in a \\parbox body that starts with a line break" in {
      // [t] makes the parbox a plain vertical box on the line, which the run search descends into
      lead("\\noindent\\parbox[t]{\\linewidth}{\nWord and more.\n}\n") shouldBe
        lead("\\noindent\\parbox[t]{\\linewidth}{Word and more.\n}\n")
    }

    "in a minipage body that starts with a line break" in {
      lead("\\noindent\\beginminipage[t]{80pt}\nWord and more.\n\\endminipage\n") shouldBe
        lead("\\noindent\\beginminipage[t]{80pt}Word and more.\n\\endminipage\n")
    }

    "a line break between words is still an interword space" in {
      val joined = lines("\\noindent{\nWord\nmore.}\n").head
      joined.boxes.exists(_.isSpace) shouldBe true
    }

    "a blank line is still a paragraph break" in {
      lines("\\noindent One.\n\n\\noindent Two.\n").length shouldBe 2
    }

    "a line break after the last word sets nothing at the paragraph's end" in {
      def shape(src: String) = lines(src).head.boxes.map(b => (b.getClass.getSimpleName, b.width)).toList
      shape("\\noindent Word.\n\n") shouldBe shape("\\noindent Word.\\par")
    }
  }
