package io.github.edadma.texish

import scala.collection.mutable

/** The font files texish carries inside its own artifact, decoded on demand from the base64 the build embeds in
  * [[EmbeddedFontData]]. This is what makes the engine work as a plain library dependency: with no font tree on
  * disk and nothing configured, the default body and math faces are still there, because their bytes are part of
  * the compiled code. A browser has no filesystem at all and a Native binary has no resource loading, so bytes
  * compiled into the program are the only form that reaches every target.
  *
  * Only the Latin Modern core is embedded — the full bundled set is far too large to carry in every artifact.
  * Everything else is read from disk when a font tree is present (see `Typesetter.resolveFontPath`) and simply
  * absent when it is not. A path here is the same relative name the bundled loads use, so disk and embed are two
  * sources for one namespace and a font tree on disk shadows the embedded copy.
  *
  * Decoded results are cached: a multi-pass document constructs a typesetter per pass, and re-decoding 2.9MB of
  * base64 each time is pure waste.
  */
private[texish] object EmbeddedFonts:

  private val cache = mutable.HashMap.empty[String, Array[Byte]]

  /** True when `path` is one of the embedded fonts. */
  def has(path: String): Boolean = EmbeddedFontData.chunks.contains(path)

  /** The decoded bytes of an embedded font, or None when the path is not one of them. */
  def get(path: String): Option[Array[Byte]] =
    if has(path) then Some(cache.getOrElseUpdate(path, decodeBase64(EmbeddedFontData.chunks(path).mkString)))
    else None

  private val table: Array[Int] =
    val t  = Array.fill(128)(-1)
    val al = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
    for (c, i) <- al.zipWithIndex do t(c.toInt) = i
    t

  /** Decode standard base64 (no line breaks, optional `=` padding) to bytes. Pure Scala so it runs identically on
    * every target, without relying on `atob` or on a particular `java.util.Base64` linking. */
  private def decodeBase64(s: String): Array[Byte] =
    val len = s.length
    var pad = 0
    if len >= 1 && s.charAt(len - 1) == '=' then pad += 1
    if len >= 2 && s.charAt(len - 2) == '=' then pad += 1
    val outLen = (len / 4) * 3 - pad
    val out    = new Array[Byte](outLen)
    var i      = 0
    var o      = 0
    while i < len do
      val c0 = table(s.charAt(i).toInt)
      val c1 = table(s.charAt(i + 1).toInt)
      val c2 = table(s.charAt(i + 2).toInt) // -1 for the '=' pad character
      val c3 = table(s.charAt(i + 3).toInt)
      val n  = (c0 << 18) | (c1 << 12) | ((if c2 < 0 then 0 else c2) << 6) | (if c3 < 0 then 0 else c3)
      out(o) = ((n >> 16) & 0xff).toByte
      o += 1
      if c2 >= 0 && o < outLen then
        out(o) = ((n >> 8) & 0xff).toByte
        o += 1
      if c3 >= 0 && o < outLen then
        out(o) = (n & 0xff).toByte
        o += 1
      i += 4
    out
