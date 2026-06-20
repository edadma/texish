package io.github.edadma.texish

/** The SVG backend as it runs in the browser. It is exactly the shared [[SvgTypesetter]] — same outline-fill
  * drawing, same markup — with two things adapted for a host that has no filesystem: font bytes come from the
  * fonts embedded in the build ([[EmbeddedFonts]]) rather than from disk, and only the curated stack those
  * fonts cover is registered, since shipping all 61MB of bundled faces to a web page is not viable.
  *
  * The registered set is the Latin Modern family — roman body text in its four core styles plus a slanted cut,
  * the monospaced face for `\texttt`/`\url`/code, and Latin Modern Math for math mode — which is the same
  * Computer Modern look the PDF defaults to. A document that asks for a face outside this set fails with a
  * clear "not embedded" error rather than rendering silently wrong; widening the set is a matter of adding the
  * file to both the build's embed list and the list below. */
class SvgTypesetterJS extends SvgTypesetter with EmbeddedFontSet:

  // Crop each page to its ink: web output is wanted at the natural size of the content, like an inline math
  // snippet, not as a full page shrunk to fit.
  cropToContent = true

  override protected def readFontBytes(path: String): Array[Byte] = EmbeddedFonts.bytes(path)
