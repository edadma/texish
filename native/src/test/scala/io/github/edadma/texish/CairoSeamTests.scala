package io.github.edadma.texish

import io.github.edadma.libcairo.Surface
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import scala.scalanative.unsafe.*

/** Exercises the picture-graphics seam directly on a live Cairo image surface (the y-up flip and op replay are
  * the PictureBox's job, tested later — here the seam is driven in raw device coordinates). At dpi 72 one
  * device pixel is one point, so seam coordinates map 1:1 to pixels on a 100×100 page.
  */
class CairoSeamTests extends AnyFreeSpec with Matchers:

  private def page(): (CairoImageTypesetter, Surface) =
    val t = new CairoImageTypesetter(72)
    t.set("paperwidth", 100.0)
    t.set("paperheight", 100.0)
    (t, t.createPageTarget.asInstanceOf[Surface])

  // ARGB32 image data is native-endian; on this platform the bytes are laid out B, G, R, A.
  private def rgb(s: Surface, x: Int, y: Int): (Int, Int, Int) =
    s.flush()
    val p = s.getData + y.toLong * s.getStride + x.toLong * 4
    ((p(2) & 0xff), (p(1) & 0xff), (p(0) & 0xff))

  private def luminance(s: Surface, x: Int, y: Int): Int =
    val (r, g, b) = rgb(s, x, y)
    (r + g + b) / 3

  "a filled path lays down its fill colour inside and leaves the page white outside" in {
    val (t, s) = page()

    t.setColor(Color("red"))
    t.newPath()
    t.moveTo(20, 20); t.lineTo(60, 20); t.lineTo(60, 60); t.lineTo(20, 60); t.closePath()
    t.fillPath(false)

    val (r, g, b) = rgb(s, 40, 40)
    r should be > 200
    g should be < 60
    b should be < 60

    luminance(s, 5, 5) should be > 240
  }

  "a stroked path lays down its stroke colour along the line at the set width" in {
    val (t, s) = page()

    t.setColor(Color("blue"))
    t.setLineWidth(6)
    t.newPath(); t.moveTo(10, 50); t.lineTo(90, 50); t.strokePath()

    val (r, _, b) = rgb(s, 50, 50)
    b should be > 200
    r should be < 60
    luminance(s, 50, 10) should be > 240 // well off the line
  }

  "transforms compose and gsave/grestore unwinds them" in {
    val (t, s) = page()

    t.setColor(Color("black"))
    t.gsave()
    t.translate(50, 50)
    t.newPath(); t.moveTo(0, 0); t.lineTo(10, 0); t.lineTo(10, 10); t.lineTo(0, 10); t.closePath(); t.fillPath(false)
    t.grestore()

    luminance(s, 55, 55) should be < 60 // square landed at the translated origin
    luminance(s, 5, 5) should be > 240  // nothing at the real origin
  }

  "a dashed stroke leaves gaps a solid one does not" in {
    def inkOnRow(dash: Boolean): Int =
      val (t, s) = page()
      t.setColor(Color("black"))
      t.setLineWidth(2)
      if dash then t.setDash(Seq(4.0, 4.0), 0)
      t.newPath(); t.moveTo(5, 50); t.lineTo(95, 50); t.strokePath()
      (0 until 100).count(x => luminance(s, x, 50) < 128)

    inkOnRow(dash = true) should be < inkOnRow(dash = false)
  }
