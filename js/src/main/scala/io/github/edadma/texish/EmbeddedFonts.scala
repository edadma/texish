package io.github.edadma.texish

import scala.collection.mutable

/** The font bytes the in-browser renderer draws with, decoded on demand from the base64 the build embeds in
  * [[EmbeddedFontData]]. A browser has no filesystem, so the Scala.js [[SvgTypesetterJS]] reads its fonts from
  * here instead of from disk. Decoded results are cached, since the same font is requested for every render. */
private[texish] object EmbeddedFonts:

  private val cache = mutable.HashMap.empty[String, Array[Byte]]

  /** True when `path` is one of the embedded fonts. */
  def has(path: String): Boolean = EmbeddedFontData.chunks.contains(path)

  /** The decoded bytes of an embedded font, or a clear error if the path is not one of the shipped fonts. */
  def bytes(path: String): Array[Byte] =
    cache.getOrElseUpdate(
      path,
      EmbeddedFontData.chunks.get(path) match
        case Some(parts) => decodeBase64(parts.mkString)
        case None =>
          sys.error(s"font '$path' is not embedded in the browser build (only the Latin Modern stack ships)")
    )

  private val table: Array[Int] =
    val t  = Array.fill(128)(-1)
    val al = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
    for (c, i) <- al.zipWithIndex do t(c.toInt) = i
    t

  /** Decode standard base64 (no line breaks, optional `=` padding) to bytes. Pure Scala so it runs the same in
    * a browser and under Node, without relying on `atob` or a particular `java.util.Base64` linking. */
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
