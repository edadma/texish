package io.github.edadma.texish.parser

import io.github.edadma.texish.{HeadlessTypesetter, Typesetter}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** A box's LAST paragraph must be broken while the box's own scope still stands.
  *
  * TeX ends a box by breaking the paragraph (end_graf) and only then restoring the group (unsave), so a
  * parameter assigned inside the box governs every paragraph in it, the last one included. texish reads a
  * box body's braces as ordinary scope tokens, so processing the body whole would pop the scope before the
  * final paragraph was broken -- and that paragraph alone would then be set under the enclosing values.
  *
  * The failure is silent: no error, correct output for every paragraph but the last, and a wrong result only
  * where the inner and outer values differ. It reached print in a Z-fold tract whose back face ran at 21pt
  * leading and then dropped to the enclosing 16.8pt partway down its third panel.
  *
  * The HeadlessTypesetter gives every character width 6 and metrics ascent 8 / descent 2, so a box's height
  * is an exact function of its baselines and these comparisons are not approximate.
  */
class BoxParagraphScopeTests extends AnyFreeSpec with Matchers:

  private def fixture(): Processor =
    val t       = new HeadlessTypesetter
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)
    proc

  // Four words at width 60 (ten characters to the line) break to four lines, so a box built from them has
  // three baselineskips in it and its height moves visibly with the value in force.
  private val body = "AAAAA BBBBB CCCCC DDDDD EEEEE FFFFF GGGGG HHHHH"

  "baselineskip set inside a box governs the box's only paragraph" in {
    val proc = fixture()
    proc.process(
      s"""\\set hsize {60} \\set baselineskip {12}
         |\\setbox inside \\vbox{\\set baselineskip {40} $body}
         |\\set baselineskip {40}
         |\\setbox outside \\vbox{$body}
         |\\set baselineskip {12}
         |\\set hi {\\ht inside} \\set ho {\\ht outside}""".stripMargin,
    )
    // Setting it inside the box and setting it outside must agree: same text, same measure, same leading.
    proc.handler.get("hi") shouldBe proc.handler.get("ho")
  }

  "it governs the LAST paragraph too, not only the earlier ones" in {
    val proc = fixture()
    proc.process(
      s"""\\set hsize {60} \\set baselineskip {12}
         |\\setbox inside \\vbox{\\set baselineskip {40} $body
         |
         |$body}
         |\\set baselineskip {40}
         |\\setbox outside \\vbox{$body
         |
         |$body}
         |\\set baselineskip {12}
         |\\set hi {\\ht inside} \\set ho {\\ht outside}""".stripMargin,
    )
    // This is the case that regressed: the first paragraph was broken by the \par between them, while the
    // scope still stood, so it was always correct. Only the second one was set under the outer 12.
    proc.handler.get("hi") shouldBe proc.handler.get("ho")
  }

  "an explicit \\par before the closing brace was the old workaround and still agrees" in {
    val proc = fixture()
    proc.process(
      s"""\\set hsize {60} \\set baselineskip {12}
         |\\setbox withpar \\vbox{\\set baselineskip {40} $body\\par}
         |\\setbox without \\vbox{\\set baselineskip {40} $body}
         |\\set hp {\\ht withpar} \\set hw {\\ht without}""".stripMargin,
    )
    // Ending the paragraph inside the group was the documented way around this. It must keep working, and
    // must now give the same answer as leaving it out.
    proc.handler.get("hp") shouldBe proc.handler.get("hw")
  }

  "a box body with no braces is unaffected" in {
    val proc = fixture()
    proc.process("\\set hsize {60} \\set baselineskip {12} \\setbox b \\hbox{AB} \\set w {\\wd b}")
    proc.handler.get("w") shouldBe Value.Dimen(12.0)
  }
