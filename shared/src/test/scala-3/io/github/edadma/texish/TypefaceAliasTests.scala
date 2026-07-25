package io.github.edadma.texish

import scala.collection.mutable

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** A typeface with two names — `\font hindi` for the Devanagari family, `\font korean` for the Korean CJK one —
  * shares the faces the family already opened. Registering the alias by loading the same files a second time
  * would leave the backend holding two faces over one file, which for the large CJK cuts is a real duplication.
  */
class TypefaceAliasTests extends AnyFreeSpec with Matchers:

  /** Records every path the backend is asked to open. `lazy` because the bundled fonts are loaded from the
    * superclass constructor, before a plain `val` of a subclass would be initialized.
    */
  private class RecordingTypesetter extends HeadlessTypesetter:
    lazy val opened: mutable.ArrayBuffer[String] = mutable.ArrayBuffer.empty

    override def loadFont(path: String): FontFace =
      opened += path
      super.loadFont(path)

  "no bundled font file is opened more than once" in {
    val t          = new RecordingTypesetter
    val duplicated = t.opened.groupBy(identity).filter(_._2.size > 1).keys.toSeq.sorted
    duplicated shouldBe empty
  }

  "every alias resolves, and to the same face as the family it names" in {
    val t = new RecordingTypesetter
    for (alias, family) <- Seq(
                             "hindi"    -> "devanagari",
                             "assamese" -> "bengali",
                             "punjabi"  -> "gurmukhi",
                             "japanese" -> "cjkjp",
                             "korean"   -> "cjkkr",
                           )
    do
      withClue(s"\\font $alias: ") {
        t.makeFont(alias, 12, Set.empty).renderFont shouldBe t.makeFont(family, 12, Set.empty).renderFont
        t.makeFont(alias, 12, Set("bold")).renderFont shouldBe t.makeFont(family, 12, Set("bold")).renderFont
        // the bold cut is a real one, not the regular face standing in for it
        t.makeFont(alias, 12, Set("bold")).renderFont should not be t.makeFont(alias, 12, Set.empty).renderFont
      }
  }

  "aliasing an unknown typeface is an error, as is aliasing over a loaded one" in {
    val t = new RecordingTypesetter
    a[TexishException] should be thrownBy t.aliasTypeface("shorthand", "nosuchfamily")
    a[TexishException] should be thrownBy t.aliasTypeface("hindi", "devanagari") // already taken
  }
