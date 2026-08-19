package io.github.edadma.texish

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** The core the build compiles into the artifact as base64. It is what makes texish work as a plain library
  * dependency: with no font tree on disk and nothing configured, these bytes are the only faces there are, so
  * everything `loadCoreFonts` promises must be complete within them.
  *
  * These tests run with the source tree's `fonts/` folder present, so the engine here loads from disk — which is
  * exactly what makes the coverage tests below worth having. They compare what the core loads register against
  * what the embed carries, in both directions, and so catch the failure that is otherwise invisible until
  * someone runs with no font tree: a cut added to `loadCoreFonts` and not to the build's embed list, which
  * quietly disappears from every zero-configuration install.
  */
class EmbeddedCoreTests extends AnyFreeSpec with Matchers:

  /** The families the engine guarantees on every host — the body super-family with its mono and sans roles, the
    * math face, the glyph-fallback face, and the code face `\code` sets a listing in. */
  private val CoreFamilies = Typesetter.CoreFamilies

  /** The packages compiled into the artifact: what a document needs to be an ordinary document, plus `qrcode`,
    * which was an engine primitive until it became a package and must not quietly stop working for anyone who
    * installs the binary alone. */
  private val CorePackages = Set("base", "document", "qrcode")


  /** A typesetter that opens the paths the way any host does, and lets a test read back what got registered.
    * Constructed without the catalogue, so what it holds is exactly what `loadCoreFonts` registered — the
    * catalogue extends `jetbrains` with the rest of its weight range, and those cuts are not core. */
  private class Reader extends HeadlessTypesetter(catalogue = false):
    def facesOf(typeface: String): Set[String] =
      typefaces.get(typeface).map(_.fonts.values.map(_._1).toSet).getOrElse(Set.empty)

  /** The bundled name of a face, whatever root it was resolved under — the registered face is the resolved path
    * on this backend, and only the part from `fonts/` on names the font. */
  private def bundledName(face: String): String =
    val i = face.indexOf("fonts/")
    if i >= 0 then face.substring(i) else face

  "every embedded font decodes to bytes an sfnt reader would accept" in {
    val paths = EmbeddedFontData.chunks.keys.toSeq

    paths should not be empty

    for path <- paths do
      withClue(s"$path: ") {
        val bytes = EmbeddedFonts.get(path).getOrElse(fail(s"$path is listed but does not decode"))

        bytes.length should be > 1000
        // The sfnt version tag: "OTTO" for a CFF outline font, 0x00010000 for TrueType glyf outlines. A
        // mis-split or mis-decoded base64 chunk would land anything else here.
        val tag = bytes.take(4).map(_ & 0xff)
        tag should (be(Seq(0x4f, 0x54, 0x54, 0x4f)) or be(Seq(0x00, 0x01, 0x00, 0x00)))
      }
  }

  "a path that is not part of the core is simply absent" in {
    EmbeddedFonts.has("fonts/NotoSerifCJK/NotoSerifSC-Regular.ttf") shouldBe false
    EmbeddedFonts.get("fonts/NotoSerifCJK/NotoSerifSC-Regular.ttf") shouldBe None
    EmbeddedFonts.get("") shouldBe None
  }

  // Decoding is not cheap and a multi-pass document builds a typesetter per pass, so the same request must not
  // re-decode the whole core each time.
  "decoded bytes are cached rather than decoded again" in {
    val path  = "fonts/LatinModernRoman/lmroman10-regular.otf"
    val first = EmbeddedFonts.get(path).get

    EmbeddedFonts.get(path).get should be theSameInstanceAs first
  }

  // The failure this guards: a new core cut loaded from disk but never added to the embed list renders fine for
  // anyone with the font tree and vanishes for everyone without it.
  "every face of every core family is in the embed" in {
    val t = new Reader

    for family <- CoreFamilies do
      val faces = t.facesOf(family)

      withClue(s"$family registered no faces at all: ") { faces should not be empty }

      for face <- faces do
        withClue(s"$family face ${bundledName(face)} is loaded but not embedded: ") {
          EmbeddedFonts.has(bundledName(face)) shouldBe true
        }
  }

  // The converse: a path carried in every artifact that nothing loads is dead weight in every artifact.
  "the embed carries nothing outside the core families" in {
    val t    = new Reader
    val used = CoreFamilies.flatMap(t.facesOf).map(bundledName).toSet

    EmbeddedFontData.chunks.keySet.diff(used) shouldBe empty
  }

  // The core is what a host gets for free, so it is worth stating outright rather than only as a set relation.
  // Glyph fallback in particular is easy to lose by accident: it is configured from a face that was for a long
  // time loaded from disk only, so an embed that dropped it would leave every zero-configuration install setting
  // missing-glyph boxes for the first Greek or Cyrillic word, with nothing to point at the cause.
  "the guaranteed baseline includes a fallback face" in {
    val t = new Reader

    t.fallbackTypeface shouldBe Some("newcm")
    t.facesOf("newcm") should have size 4 // regular, bold, italic, bold-italic — a substitution keeps its weight
  }

  // The embedded packages are a whitelist: what a document needs to be an ordinary document, and nothing whose
  // own font requirements the embed cannot meet. `music` is the clearest case of the second — it sets notation
  // from a SMuFL face, and those are catalogue fonts, so embedding it would ship a module that resolves and then
  // cannot draw a note. `qrcode` is on the list for the opposite reason twice over: it draws rectangles and needs
  // no face at all, and it replaced an engine primitive, so leaving it out would mean a binary that used to draw
  // a QR code from a bare install and silently stopped.
  "the embedded packages are exactly the core set" in {
    EmbeddedPackages.sources.keySet shouldBe CorePackages
    EmbeddedFontData.chunks.keys.filter(_.contains("Bravura")) shouldBe empty
    EmbeddedFontData.chunks.keys.filter(_.contains("Petaluma")) shouldBe empty
  }

  // The set has to be closed under \use, or an embedded package resolves and then fails on its dependency —
  // which is a worse failure than not resolving at all, because it happens deeper in.
  "every package the embedded ones depend on is embedded too" in {
    val used = """\\use\{([a-z]+)\}""".r

    for name <- CorePackages do
      val source = EmbeddedPackages.sources(name).mkString

      for m <- used.findAllMatchIn(source) do
        withClue(s"$name uses ${m.group(1)}, which is not embedded: ") {
          CorePackages should contain(m.group(1))
        }
  }
