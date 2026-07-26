package io.github.edadma.texish

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** The split between what texish guarantees and what it merely bundles.
  *
  * The core is compiled into the artifact, so a program embedding texish as a library has a working engine having
  * configured nothing. The catalogue — the complex-script faces, the CJK cuts, the alternative text families — is
  * far too large for that and comes from a font tree on disk, which means a host must both point the engine at one
  * and ask for the families. Neither half of that is guessable from a rendering failure, so the interesting
  * behaviour here is not that a missing family fails: it is *what it says when it fails*.
  */
class BundledCatalogueTests extends AnyFreeSpec with Matchers:

  /** A typesetter as a library consumer gets one: the embedded core and nothing else, whatever this machine
    * happens to have in its filesystem. Constructed without the catalogue, then stripped of the sources the
    * constructor seeds from the current directory and the environment. */
  private def bare(): HeadlessTypesetter =
    val t = new HeadlessTypesetter(catalogue = false)

    t.clearFontSources()
    t

  "the core families are there with no font tree and no catalogue" in {
    val t = bare()

    for family <- Seq("lmroman", "lmmath", "newcm", "bravura") do
      withClue(s"$family: ") { noException should be thrownBy t.makeFont(family, 10, Set.empty) }

    // The roles of the body family come out of the embed too, not just its roman.
    noException should be thrownBy t.makeFont("lmroman", 10, Set("mono"))
    noException should be thrownBy t.makeFont("lmroman", 10, Set("sans", "bold"))
    noException should be thrownBy t.makeFont("lmroman", 10, Set("smallcaps"))
  }

  // The two ways a bundled family can be missing read very differently to whoever has to fix it: one is an
  // installation that never asked for the catalogue, the other an installation whose font tree does not have the
  // files. Neither is a misspelling, and saying "not found" for all three sends people to the wrong place.
  "a bundled family that was never loaded says the catalogue was not loaded" in {
    val t = new HeadlessTypesetter(catalogue = false)

    val e = the[TexishException] thrownBy t.makeFont("hebrew", 10, Set.empty)

    e.getMessage should include("'hebrew'")
    e.getMessage should include("texish bundles")
    e.getMessage should include("loadBundledCatalogue()")
  }

  "a bundled family whose files no source has says so, and names a file it wanted" in {
    val t = bare()

    t.loadBundledCatalogue()

    val e = the[TexishException] thrownBy t.makeFont("hebrew", 10, Set.empty)

    e.getMessage should include("'hebrew'")
    e.getMessage should include("texish bundles")
    e.getMessage should include("fonts/NotoSerifHebrew")
    e.getMessage should not include "loadBundledCatalogue()"
  }

  // An alias is registered against a family that was skipped, so it has no face of its own to point at. Without
  // carrying the note across, \font hindi would read as a name texish has never heard of.
  "an alias of a skipped family reports what the family it names would" in {
    val t = bare()

    t.loadBundledCatalogue()

    val e = the[TexishException] thrownBy t.makeFont("hindi", 10, Set.empty)

    e.getMessage should include("'hindi'")
    e.getMessage should include("fonts/NotoSerifDevanagari")
  }

  "a name texish does not bundle is still just not found" in {
    for t <- Seq(bare(), new HeadlessTypesetter()) do
      val e = the[TexishException] thrownBy t.makeFont("garamnod", 10, Set.empty)

      e.getMessage should include("'garamnod'")
      e.getMessage should not include "texish bundles"
      e.getMessage should not include "loadBundledCatalogue()"
  }

  // Typesetter.BundledFamilies is a written-down copy of what the catalogue's code does, needed because a host
  // that never loaded the catalogue still has to recognise its names. A family added to one and not the other
  // silently loses its diagnostic, which is the whole point of having it.
  "the written-down catalogue names match the families the catalogue actually attempts" in {
    val t = new HeadlessTypesetter()

    t.attemptedCatalogueFamilies shouldBe Typesetter.BundledFamilies
  }

  "no core family is listed as bundled, since a core family can never be missing" in {
    Typesetter.BundledFamilies.intersect(Set("lmroman", "lmmath", "newcm", "bravura")) shouldBe empty
  }

  // --- sources -------------------------------------------------------------

  "a registered source supplies the catalogue families a bare engine lacks" in {
    val t = bare()

    t.registerFontSource(DirectoryFontSource("."))
    t.loadBundledCatalogue()

    noException should be thrownBy t.makeFont("hebrew", 10, Set.empty)
    t.attemptedCatalogueFamilies should contain("hebrew")
  }

  "sources are consulted in the order they were registered" in {
    val t = bare()

    // The first source to answer wins, so a face is opened from the earlier root even though both have it.
    t.registerFontSource(new FontSource:
      override def bytes(path: String): Option[Array[Byte]] = Some(Array[Byte](1, 2, 3)))
    t.registerFontSource(DirectoryFontSource("."))
    t.loadBundledCatalogue()

    // HeadlessTypesetter names a face by where it came from: a resolved path for a file, the bundled name for
    // bytes. The bytes source was registered first, so that is the name the face carries.
    t.makeFont("hebrew", 10, Set.empty).renderFont.asInstanceOf[String] shouldBe
      "fonts/NotoSerifHebrew/NotoSerifHebrew-Regular.ttf"
  }

  "a source that has nothing is passed over rather than ending the search" in {
    val t = bare()

    t.registerFontSource(new FontSource {})
    t.registerFontSource(DirectoryFontSource("."))
    t.loadBundledCatalogue()

    noException should be thrownBy t.makeFont("hebrew", 10, Set.empty)
  }

  // A document naming a file nothing has is an error, whatever the catalogue is doing — the tolerance is for the
  // catalogue's own speculative loads, not for a \loadfont the author wrote on purpose.
  "a document loading a font file no source has is an error" in {
    val t = bare()

    val e = the[TexishException] thrownBy t.loadFont("mine", "fonts/NoSuchFace.otf", Set.empty, Set.empty)

    e.getMessage should include("fonts/NoSuchFace.otf")
  }
