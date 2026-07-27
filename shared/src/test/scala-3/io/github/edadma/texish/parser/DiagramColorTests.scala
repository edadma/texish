package io.github.edadma.texish.parser

import io.github.edadma.path.Path
import io.github.edadma.texish.{Box, CharBox, Color, DocumentMode, HBox, HeadlessTypesetter, Typesetter, VBox}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable.ArrayBuffer

/** The diagram package settles its two colours against the document it is drawn in, rather than naming literals.
  *
  * It used to name them, and a literal suits one background only: the ink was a dark slate, so on a dark page the
  * edges and outlines were drawn in very nearly the page's own colour and simply were not there, while the fill
  * stayed pale, so the node labels — set in the document's pen, which is light on a dark page — disappeared into
  * it. Both failures are invisible to a test that only asks whether the document rendered.
  *
  * The package is read from the repository's own `packages/`, so this exercises the file that ships.
  */
class DiagramColorTests extends AnyFreeSpec with Matchers:

  private def repoPackages: Path =
    val here = Path(".").toAbsolutePath.normalize

    // The suite runs from the repository root under sbt; accept a nested working directory too.
    Iterator
      .iterate(here)(_.parent.orNull)
      .takeWhile(_ != null)
      .take(4)
      .map(_ / "packages")
      .find(p => (p / "diagram.texish").exists)
      .getOrElse(fail(s"cannot find the packages directory from $here"))

  private class CapturingDocument(t: HeadlessTypesetter) extends DocumentMode(t):
    val shipped = new ArrayBuffer[VBox]
    override infix def add(box: Box): Unit =
      shipped += box.asInstanceOf[VBox]
      super.add(box)

  /** Run `src` with the diagram package available, and return everything it typeset. */
  private def run(src: String): String =
    val original = Typesetter.home
    val t        = new HeadlessTypesetter
    val handler  = new TypesetterHandler(t)
    val proc     = new Processor(handler)

    registerTypesettingPrimitives(proc, handler)

    val doc = new CapturingDocument(t)

    t.document = doc

    try
      Typesetter.home = repoPackages.parent.get.toPlatformString
      Console.withOut(new java.io.ByteArrayOutputStream)(proc.process("\\set raggedbottom {1}\n" + src + "\n"))
      t.end()
    finally Typesetter.home = original

    def chars(b: Box): List[String] = b match
      case c: CharBox => List(c.text)
      case h: HBox    => h.boxes.toList.flatMap(chars)
      case v: VBox    => v.boxes.toList.flatMap(chars)
      case _          => Nil

    doc.shipped.toList.flatMap(chars).mkString

  /** The ink and fill the package settles on, for a page and pen given as hex codes. */
  private def resolved(page: String, pen: String): (String, String) =
    val out = run(s"\\use{diagram}\\pagecolor{$page}\\color{$pen}\\dgresolve\\dgink|\\dgpaint")

    out.split('|') match
      case Array(ink, paint) => (ink, paint)
      case other             => fail(s"expected 'ink|paint', got ${other.mkString("[", ", ", "]")}")

  "on a white page the ink and fill are unchanged from the colours the package used to name" in {
    val (ink, paint) = resolved("#ffffff", "#000000")

    // The fill is the same cool grey-blue as the literal it replaced, to the byte.
    paint shouldBe "#eef3fb"
    // The ink is now the document's pen rather than the old #1f2933 slate, so a diagram inks like its text.
    ink shouldBe "#000000"
  }

  "on a dark page the ink is the light pen, not a slate that would vanish into it" in {
    val (ink, _) = resolved("#1e2229", "#e8eaed")

    ink shouldBe "#e8eaed"
  }

  // The label failure, which is the half that a visible-edges fix alone would leave behind: labels are drawn in
  // the document's pen, so a fill must stay on the page's side of it or the text is lost in the node.
  "on a dark page the fill stays near the page, so a light label still reads against it" in {
    val (_, paint) = resolved("#1e2229", "#e8eaed")

    val page = Color("#1e2229")
    val fill = Color(paint)
    val pen  = Color("#e8eaed")

    def lightness(c: Color) = 0.2126 * c.red + 0.7152 * c.green + 0.0722 * c.blue

    lightness(fill) should be > lightness(page)          // lifted off the page, so the node reads as a shape
    lightness(fill) should be < lightness(pen) / 2       // still far darker than the pen that writes on it
  }

  "a colour the document sets explicitly wins over both" in {
    val out = run("\\use{diagram}\\set dgfill {#123456}\\set dglinecolor {#654321}\\dgresolve\\dgink|\\dgpaint")

    out shouldBe "#654321|#123456"
  }
