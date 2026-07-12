package io.github.edadma.texish

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets.ISO_8859_1
import java.nio.file.{Files, Paths}

/** Tags a finished Cairo PDF as sRGB by appending an sRGB `OutputIntent`.
  *
  * Cairo embeds images as untagged `/DeviceRGB` and offers no colour-management API, so an sRGB image comes out
  * of a colour-managed viewer looking shifted next to the original. Declaring a document-wide sRGB OutputIntent
  * tells the viewer to interpret DeviceRGB as sRGB, which restores faithful colour without touching the pixels.
  *
  * There is no Cairo call for this, so it is done as a PDF incremental update to the file Cairo already wrote.
  * That is only simple because the surface is restricted to PDF 1.4 (see [[CairoPDFTypesetter]]), which makes
  * Cairo write a classic cross-reference table and plaintext objects: the update appends an ICC-profile stream,
  * an OutputIntent dictionary, and a superseding copy of the catalog that references it, then a small xref
  * section chained to the original with `/Prev`. Any failure is swallowed — colour tagging must never turn a
  * successful render into a failed one.
  */
object SrgbOutputIntent:

  // A compact sRGB v2 ICC profile (lcms2's built-in sRGB, 588 bytes), embedded as hex so it ships in the binary.
  // It is the OutputIntent's destination profile: the colour space the file's DeviceRGB values are taken to be.
  private val profileHex =
    "0000024c6c636d73044000006d6e74725247422058595a2007ea0007000c000300300023616373704150504c000000000000000000000000000000" +
      "0000000000000000000000f6d6000100000000d32d6c636d730000000000000000000000000000000000000000000000000000000000000000000000" +
      "0000000000000000000000000b64657363000001080000003663707274000001400000004c777470740000018c000000146368616400" +
      "0001a00000002c7258595a000001cc000000146258595a000001e0000000146758595a000001f4000000147254524300000208000000206754524300" +
      "000208000000206254524300000208000000206368726d00000228000000246d6c756300000000000000010000000c656e55530000001a0000001c00" +
      "730052004700420020006200750069006c0074002d0069006e00006d6c756300000000000000010000000c656e5553000000300000001c004e006f00" +
      "200063006f0070007900720069006700680074002c002000750073006500200066007200650065006c007958595a20000000000000f6d60001000000" +
      "00d32d736633320000000000010c42000005defffff325000007930000fd90fffffba1fffffda2000003dc0000c06e58595a200000000000006fa000" +
      "0038f50000039058595a20000000000000249f00000f840000b6c358595a2000000000000062970000b787000018d9706172610000000000030000" +
      "000266660000f2a700000d59000013d000000a5b6368726d00000000000300000000a3d70000547b00004ccd0000999a0000266600000f5c"

  private def profile: Array[Byte] =
    val out = new Array[Byte](profileHex.length / 2)
    var i   = 0
    while i < out.length do
      out(i) = Integer.parseInt(profileHex.substring(i * 2, i * 2 + 2), 16).toByte
      i += 1
    out

  // The first run of digits after `key` at or after `from`, or None. Used to read object numbers and offsets out
  // of the trailer and the startxref line without a regex engine.
  private def intAfter(s: String, key: String, from: Int): Option[Int] =
    val k = s.indexOf(key, from)
    if k < 0 then None
    else
      var i = k + key.length
      while i < s.length && (s(i) == ' ' || s(i) == '\n' || s(i) == '\r' || s(i) == '\t') do i += 1
      val start = i
      while i < s.length && s(i).isDigit do i += 1
      if i > start then Some(s.substring(start, i).toInt) else None

  def inject(path: String): Unit =
    try
      val original = Files.readAllBytes(Paths.get(path))
      // The structure is all ASCII; ISO-8859-1 keeps one char per byte so string indices are byte offsets.
      val s          = new String(original, ISO_8859_1)
      val trailerAt  = s.lastIndexOf("trailer")
      val startxrefN = s.lastIndexOf("startxref")
      if trailerAt < 0 || startxrefN < 0 then return

      val rootNum  = intAfter(s, "/Root", trailerAt).getOrElse(return)
      val size     = intAfter(s, "/Size", trailerAt).getOrElse(return)
      val prevXref = intAfter(s, "startxref", startxrefN).getOrElse(return)
      val infoNum  = intAfter(s, "/Info", trailerAt)

      // The catalog object and the balanced extent of its dictionary, so it can be reissued with the new key.
      val objAt = s.indexOf(s"$rootNum 0 obj")
      if objAt < 0 then return
      val dictOpen = s.indexOf("<<", objAt)
      if dictOpen < 0 then return
      var depth     = 0
      var j         = dictOpen
      var dictClose = -1
      while j < s.length - 1 && dictClose < 0 do
        val two = s.substring(j, j + 2)
        if two == "<<" then { depth += 1; j += 2 }
        else if two == ">>" then { depth -= 1; if depth == 0 then dictClose = j else j += 2 }
        else j += 1
      if dictClose < 0 then return
      val catalogInner = s.substring(dictOpen + 2, dictClose)

      val iccNum = size
      val oiNum  = size + 1
      val icc    = profile

      val iccHead = s"$iccNum 0 obj\n<< /N 3 /Alternate /DeviceRGB /Length ${icc.length} >>\nstream\n".getBytes(ISO_8859_1)
      val iccTail = "\nendstream\nendobj\n".getBytes(ISO_8859_1)
      val oiObj =
        s"$oiNum 0 obj\n<< /Type /OutputIntent /S /GTS_PDFA1 /OutputConditionIdentifier (sRGB IEC61966-2.1) /Info (sRGB IEC61966-2.1) /DestOutputProfile $iccNum 0 R >>\nendobj\n"
          .getBytes(ISO_8859_1)
      val catObj = s"$rootNum 0 obj\n<<$catalogInner /OutputIntents [$oiNum 0 R] >>\nendobj\n".getBytes(ISO_8859_1)

      val offIcc  = original.length
      val iccLen  = iccHead.length + icc.length + iccTail.length
      val offOi   = offIcc + iccLen
      val offCat  = offOi + oiObj.length
      val newXref = offCat + catObj.length

      def entry(off: Int): String = f"$off%010d 00000 n \n"
      val info = infoNum.map(n => s" /Info $n 0 R").getOrElse("")
      val xref = new StringBuilder
      xref.append("xref\n")
      xref.append(s"$rootNum 1\n").append(entry(offCat))
      xref.append(s"$iccNum 2\n").append(entry(offIcc)).append(entry(offOi))
      xref.append("trailer\n")
      xref.append(s"<< /Size ${size + 2} /Root $rootNum 0 R$info /Prev $prevXref >>\n")
      xref.append(s"startxref\n$newXref\n%%EOF\n")

      val out = new ByteArrayOutputStream(newXref + xref.length + 64)
      out.write(original)
      out.write(iccHead); out.write(icc); out.write(iccTail)
      out.write(oiObj)
      out.write(catObj)
      out.write(xref.toString.getBytes(ISO_8859_1))
      Files.write(Paths.get(path), out.toByteArray)
    catch case _: Throwable => ()
