package io.github.edadma.texish

/** The SVG backend as it runs in the browser. It is exactly the shared [[SvgTypesetter]] — same outline-fill
  * drawing, same markup — differing only in cropping each page to its ink.
  *
  * A browser has no filesystem, so no font path resolves and every face comes from the Latin Modern core
  * compiled into the build (see [[EmbeddedFonts]]): roman body text in its core styles plus slanted and
  * small-caps cuts, the monospaced and sans roles, and Latin Modern Math — the same Computer Modern look the PDF
  * defaults to. A document that asks for a face outside that set gets a clear "typeface not found" rather than
  * rendering silently wrong. */
class SvgTypesetterJS extends SvgTypesetter:

  // Crop each page to its ink: web output is wanted at the natural size of the content, like an inline math
  // snippet, not as a full page shrunk to fit.
  cropToContent = true
