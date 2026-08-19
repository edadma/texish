package io.github.edadma.texish.parser

/** A short, stable fingerprint of a barcode symbol, for the fixtures in [[QrCodeTests]] and [[DataMatrixTests]]
  * that are too large to write out module by module.
  *
  * It is FNV-1a rather than a real digest because the tests run on Scala Native as well as the JVM, and
  * `java.security.MessageDigest` exists on neither Native nor a browser. Nothing here needs to resist an
  * adversary — a fingerprint is only asked whether a symbol still comes out exactly as it did when it was checked
  * against an independent encoder, and any change of one module has to change the answer.
  */
object SymbolFingerprint:
  def of(rows: Vector[String]): String =
    var h = 0xcbf29ce484222325L
    for c <- rows.mkString("\n") do h = (h ^ (c.toLong & 0xffff)) * 0x100000001b3L
    val hex = java.lang.Long.toHexString(h)
    "0" * (16 - hex.length) + hex
