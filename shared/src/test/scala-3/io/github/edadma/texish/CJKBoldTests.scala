package io.github.edadma.texish

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** The bundled Chinese faces (`cjksc`, `cjktc`) ship a bold cut alongside the regular one, so `\font cjksc 12
  * bold` sets real bold instead of LaTeX-style substituting the regular weight (the fallback in
  * Typesetter.makeFont). HeadlessTypesetter registers each face by its file path, so the resolved face is
  * enough to tell the two cuts apart — and, before the bold cut was bundled, a bold request resolved to the
  * Regular file (no "Bold" in the path), which is exactly what this would catch.
  */
class CJKBoldTests extends AnyFreeSpec with Matchers:

  private def faceOf(typeface: String, style: Set[String]): String =
    new HeadlessTypesetter().makeFont(typeface, 12.0, style).renderFont.asInstanceOf[String]

  "Simplified Chinese resolves a bold request to a distinct bold cut" in {
    faceOf("cjksc", Set.empty) should include("Regular")
    faceOf("cjksc", Set("bold")) should include("Bold")
    faceOf("cjksc", Set("bold")) should not equal faceOf("cjksc", Set.empty)
  }

  "Traditional Chinese resolves a bold request to a distinct bold cut" in {
    faceOf("cjktc", Set.empty) should include("Regular")
    faceOf("cjktc", Set("bold")) should include("Bold")
    faceOf("cjktc", Set("bold")) should not equal faceOf("cjktc", Set.empty)
  }
