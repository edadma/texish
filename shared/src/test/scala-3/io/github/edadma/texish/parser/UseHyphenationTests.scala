package io.github.edadma.texish.parser

import io.github.edadma.texish.{HeadlessTypesetter, Hyphenation, TexishException}

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** `\usehyphenation` — the one command a document uses to switch hyphenation on, whichever tier the language
  * it names happens to be in. Five languages are compiled into the artifact and the rest are files in the
  * installation's `hyphenation/` folder, and a document is not meant to know or care which is which. */
class UseHyphenationTests extends AnyFreeSpec with Matchers:

  private def run(src: String): HeadlessTypesetter =
    val t       = new HeadlessTypesetter
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)

    registerTypesettingPrimitives(proc, handler)
    Console.withOut(new java.io.ByteArrayOutputStream)(proc.process(src))
    t

  "a language texish bundles is loaded by naming it" in {
    run("\\usehyphenation{fr}").language shouldBe Some("fr")
  }

  "a language that is a file in the tree is loaded the same way" in {
    run("\\usehyphenation{de-1996}").language shouldBe Some("de-1996")
    Hyphenation.isLoaded("de-1996") shouldBe true
  }

  "a tag texish ships no patterns for says so, and says where to get some" in {
    val e = the[TexishException] thrownBy run("\\usehyphenation{qq}")

    e.getMessage should include("bundles no patterns for 'qq'")
    e.getMessage should include("\\loadhyphenation{qq}{path}")
  }

  "a bundled language whose files are missing blames the installation, not the document" in {
    // The two readings are fixed in different places — a missing hyphenation/ folder is a broken install, an
    // unknown tag is a typo — so the note is a pure function of the tag and is checked as one. In a checkout
    // the folder is always there, which is exactly why the message cannot be reached through the primitive.
    Hyphenation.unavailableNote("de-1996") should include("no hyphenation folder was found")
    Hyphenation.unavailableNote("qq") should include("bundles no patterns")
  }

  "\\language after \\usehyphenation selects an already-loaded language" in {
    run("\\usehyphenation{es}\\usehyphenation{pt}\\language{es}").language shouldBe Some("es")
  }
