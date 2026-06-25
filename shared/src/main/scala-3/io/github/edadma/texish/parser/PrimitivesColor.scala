package io.github.edadma.texish.parser

import io.github.edadma.char_reader.CharReader
import io.github.edadma.texish.Color

/** The Oklab perceptual colour space (Björn Ottosson, 2020) and its polar form Oklch. Conversions to and from sRGB
  * back the `\oklch` / `\oklchof` primitives; deriving a palette this way keeps hue and chroma steady while lightness
  * varies, which a raw RGB or even an Oklab-toward-black blend does not. */
private object Oklab:
  private val hexDigits = "0123456789abcdef"
  private def hex2(v: Int): String =
    val c = if v < 0 then 0 else if v > 255 then 255 else v
    s"${hexDigits(c >> 4)}${hexDigits(c & 15)}"
  def hex(r: Double, g: Double, b: Double): String =
    def i(v: Double) = (v * 255).round.toInt
    s"#${hex2(i(r))}${hex2(i(g))}${hex2(i(b))}"

  // cube root that keeps the sign, so it is safe for any real input
  private def cbrt(x: Double): Double = if x < 0 then -math.pow(-x, 1.0 / 3) else math.pow(x, 1.0 / 3)
  // sRGB companding (gamma) between a 0..1 component and linear light
  private def toLinear(c: Double): Double = if c <= 0.04045 then c / 12.92 else math.pow((c + 0.055) / 1.055, 2.4)
  private def toSrgb(c: Double): Double   = if c <= 0.0031308 then 12.92 * c else 1.055 * math.pow(c, 1.0 / 2.4) - 0.055

  /** sRGB (0..1) -> Oklab (L, a, b). */
  def toOklab(r0: Double, g0: Double, b0: Double): (Double, Double, Double) =
    val r = toLinear(r0); val g = toLinear(g0); val b = toLinear(b0)
    val l = cbrt(0.4122214708 * r + 0.5363325363 * g + 0.0514459929 * b)
    val m = cbrt(0.2119034982 * r + 0.6806995451 * g + 0.1073969566 * b)
    val s = cbrt(0.0883024619 * r + 0.2817188376 * g + 0.6299787005 * b)
    (
      0.2104542553 * l + 0.7936177850 * m - 0.0040720468 * s,
      1.9779984951 * l - 2.4285922050 * m + 0.4505937099 * s,
      0.0259040371 * l + 0.7827717662 * m - 0.8086757660 * s,
    )
  /** Oklab (L, a, b) -> sRGB (0..1). */
  def toRgb(bigL: Double, a: Double, b2: Double): (Double, Double, Double) =
    val l = bigL + 0.3963377774 * a + 0.2158037573 * b2
    val m = bigL - 0.1055613458 * a - 0.0638541728 * b2
    val s = bigL - 0.0894841775 * a - 1.2914855480 * b2
    val l3 = l * l * l; val m3 = m * m * m; val s3 = s * s * s
    (
      toSrgb(4.0767416621 * l3 - 3.3077115913 * m3 + 0.2309699292 * s3),
      toSrgb(-1.2684380046 * l3 + 2.6097574011 * m3 - 0.3413193965 * s3),
      toSrgb(-0.0041960863 * l3 - 0.7034186147 * m3 + 1.7076147010 * s3),
    )

/** \oklch{L}{C}{h} — build a colour from its Oklch coordinates and return it as a `#rrggbb` string, ready for \fill,
  * \stroke or \color. L is perceived lightness 0..1, C is chroma (roughly 0..0.4), h is hue in degrees. This is the
  * modern, perceptually uniform way to specify a colour; a family of shades is just one L varied with C and h held,
  * which keeps the hue true as it darkens (unlike fading toward black). Pair it with \oklchof to shade a chosen
  * colour. */
object OklchPrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    def num(label: String): Double = proc.evalArgumentExpr(pos) match
      case Value.Num(n) => n
      case other        => proc.handler.error(s"\\oklch needs a number for $label, got ${Value.display(other)}", pos)
    val l = num("L"); val c = num("C"); val h = num("h")
    val hr        = math.toRadians(h)
    val (r, g, b) = Oklab.toRgb(l, c * math.cos(hr), c * math.sin(hr))
    val result    = Oklab.hex(r, g, b)
    proc.setResult(Value.Text(result))
    proc.handler.text(result)

/** \oklchof{color} — the Oklch coordinates of a colour, as the three-element sequence `L C h` (lightness, chroma,
  * hue in degrees). Take its `\head` for the lightness, adjust it, and rebuild with `\oklch` to derive a lighter or
  * darker shade of the colour that keeps its hue. The argument is anything Color accepts — a CSS name or hex code. */
object OklchOfPrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    val col       = Color(Value.display(proc.evalArgumentExpr(pos)))
    val (l, a, b) = Oklab.toOklab(col.red, col.green, col.blue)
    val c         = math.sqrt(a * a + b * b)
    val h         = { val d = math.toDegrees(math.atan2(b, a)); if d < 0 then d + 360 else d }
    proc.setResult(Value.Seq(Vector(Value.Num(l), Value.Num(c), Value.Num(h))))
