package io.github.edadma.texish

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** The Latin Modern core the build compiles into the artifact as base64. It is what makes texish work as a plain
  * library dependency: with no font tree on disk and nothing configured, these bytes are the only faces there
  * are, so the default body family and the math family must be complete within them.
  *
  * These tests run with the source tree's `fonts/` folder present, so the engine here loads from disk — which is
  * exactly what makes the coverage test below worth having. It compares what the bundled loads register under
  * the default families against what the embed carries, and so catches the failure that is otherwise invisible
  * until someone runs with no font tree: a cut added to `loadBundledFonts` and not to the build's embed list,
  * which quietly disappears from every zero-configuration install.
  */
class EmbeddedCoreTests extends AnyFreeSpec with Matchers:

  /** A typesetter that opens the paths the way any host does, and lets a test read back what got registered. */
  private class Reader extends HeadlessTypesetter:
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
  // re-decode 2.9MB of base64 each time.
  "decoded bytes are cached rather than decoded again" in {
    val path  = "fonts/LatinModernRoman/lmroman10-regular.otf"
    val first = EmbeddedFonts.get(path).get

    EmbeddedFonts.get(path).get should be theSameInstanceAs first
  }

  // The failure this guards: a new lmroman cut loaded from disk but never added to the embed list renders fine
  // for anyone with the font tree and vanishes for everyone without it.
  "every face of the default body and math families is in the core" in {
    val t = new Reader

    for family <- Seq("lmroman", "lmmath") do
      val faces = t.facesOf(family)

      withClue(s"$family registered no faces at all: ") { faces should not be empty }

      for face <- faces do
        withClue(s"$family face ${bundledName(face)} is loaded but not embedded: ") {
          EmbeddedFonts.has(bundledName(face)) shouldBe true
        }
  }

  // The converse: a path carried in every artifact that nothing loads is dead weight in every artifact.
  "the core carries nothing the default families do not use" in {
    val t     = new Reader
    val used  = (t.facesOf("lmroman") ++ t.facesOf("lmmath")).map(bundledName)

    EmbeddedFontData.chunks.keySet.diff(used) shouldBe empty
  }
