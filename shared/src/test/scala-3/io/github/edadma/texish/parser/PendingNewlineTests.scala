package io.github.edadma.texish.parser

import io.github.edadma.texish.{Box, Builder, HBox, HeadlessTypesetter}
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
