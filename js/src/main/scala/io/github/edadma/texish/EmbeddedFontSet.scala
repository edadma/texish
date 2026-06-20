package io.github.edadma.texish

/** The set of typefaces the in-browser backends register: the Latin Modern stack the build embeds — roman
  * body text in its core styles plus a slanted cut, the monospaced face, and Latin Modern Math. Shipping all
  * 61MB of bundled faces to a web page is not viable, so a document that asks for a face outside this set
  * fails with a clear error rather than rendering silently wrong; widening it means adding the file to both the
  * build's embed list and the list below. Each concrete backend supplies the bytes (from [[EmbeddedFonts]])
  * through its own `loadFont`. */
trait EmbeddedFontSet extends Typesetter:

  override protected def loadBundledFonts(): Unit =
    val lmLigatures = "ﬃﬄﬁﬂﬀ".map(_.toString).toSet ++ Ligatures.TEXT_REPRESENTATIONS
    loadFont("lmroman", "fonts/LatinModernRoman/lmroman10-regular.otf", lmLigatures, Set.empty)
    loadFont("lmroman", "fonts/LatinModernRoman/lmroman10-bold.otf", lmLigatures, Set("bold"))
    loadFont("lmroman", "fonts/LatinModernRoman/lmroman10-italic.otf", lmLigatures, Set("italic"))
    loadFont("lmroman", "fonts/LatinModernRoman/lmroman10-bolditalic.otf", lmLigatures, Set("bold", "italic"))
    loadFont("lmroman", "fonts/LatinModernRoman/lmromanslant10-regular.otf", lmLigatures, Set("slanted"))
    loadFont("mono", "fonts/LatinModernMono/lmmono10-regular.otf", Set.empty, Set.empty)
    loadFont("mono", "fonts/LatinModernMono/lmmonolt10-bold.otf", Set.empty, Set("bold"))
    loadFont("mono", "fonts/LatinModernMono/lmmono10-italic.otf", Set.empty, Set("italic"))
    loadFont("mono", "fonts/LatinModernMono/lmmonolt10-boldoblique.otf", Set.empty, Set("bold", "italic"))
    loadFont("lmmath", "fonts/LatinModernMath/LatinModernMath-SMaFL.otf", Set(), Set())
