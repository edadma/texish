package io.github.edadma.texish

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import java.awt.image.BufferedImage

/** Exercises the picture-graphics seam directly on a live page surface (the y-up flip and op replay are the
  * PictureBox's job, tested later — here the seam is driven in raw device coordinates). At dpi 72 one device
  * pixel is one point, so seam coordinates map 1:1 to pixels on a 100×100 page.
  */
class Graphics2DSeamTests extends AnyFreeSpec with Matchers:

  private def page(): (Graphics2DTypesetter, BufferedImage) =
    val t = new Graphics2DTypesetter(dpi = 72)
    t.init(100, 100)
    (t, t.createPageTarget.asInstanceOf[BufferedImage])

  private def red(rgb: Int)   = (rgb >> 16) & 0xff
  private def green(rgb: Int) = (rgb >> 8) & 0xff
  private def blue(rgb: Int)  = rgb & 0xff
  private def luminance(rgb: Int) = (red(rgb) + green(rgb) + blue(rgb)) / 3

  "a filled path lays down its fill colour inside and leaves the page white outside" in {
    val (t, img) = page()

    t.setColor(Color("red"))
    t.newPath()
    t.moveTo(20, 20); t.lineTo(60, 20); t.lineTo(60, 60); t.lineTo(20, 60); t.closePath()
    t.fillPath(false)

    red(img.getRGB(40, 40)) should be > 200
    green(img.getRGB(40, 40)) should be < 60
    blue(img.getRGB(40, 40)) should be < 60

    luminance(img.getRGB(5, 5)) should be > 240
  }

  "a stroked path lays down its stroke colour along the line at the set width" in {
    val (t, img) = page()

    t.setColor(Color("blue"))
    t.setLineWidth(6)
    t.newPath(); t.moveTo(10, 50); t.lineTo(90, 50); t.strokePath()

    blue(img.getRGB(50, 50)) should be > 200
    red(img.getRGB(50, 50)) should be < 60
    luminance(img.getRGB(50, 10)) should be > 240 // well off the line
  }

  "transforms compose and gsave/grestore unwinds them" in {
    val (t, img) = page()

    t.setColor(Color("black"))
    t.gsave()
    t.translate(50, 50)
    t.newPath(); t.moveTo(0, 0); t.lineTo(10, 0); t.lineTo(10, 10); t.lineTo(0, 10); t.closePath(); t.fillPath(false)
    t.grestore()

    luminance(img.getRGB(55, 55)) should be < 60  // square landed at the translated origin
    luminance(img.getRGB(5, 5)) should be > 240   // nothing at the real origin
  }

  "a dashed stroke leaves gaps a solid one does not" in {
    def inkOnRow(dash: Boolean): Int =
      val (t, img) = page()
      t.setColor(Color("black"))
      t.setLineWidth(2)
      if dash then t.setDash(Seq(4.0, 4.0), 0)
      t.newPath(); t.moveTo(5, 50); t.lineTo(95, 50); t.strokePath()
      (0 until img.getWidth).count(x => luminance(img.getRGB(x, 50)) < 128)

    inkOnRow(dash = true) should be < inkOnRow(dash = false)
  }

  "getTextExtents of an empty string reports zero extents instead of crashing" in {
    // TextLayout rejects ""; the other backends return zeros, and CharBox can legitimately construct empty
    val (t, _) = page()
    val face   = t.loadFont("fonts/LatinModernRoman/lmroman10-regular.otf")
    val rf     = t.makeFont(face, 12.0)

    t.getTextExtents("", rf) shouldBe TextExtents(0, 0, 0, 0, 0, 0)
  }

  "drawLine ends at its endpoints (butt caps), matching the PDF and SVG backends" in {
    // square caps (the old AWT default) extended each end by half the line width, making every \underline
    // and rule fraction longer on this backend than on the others
    val (t, img) = page()

    t.setColor(Color("black"))
    t.setLineWidth(6)
    t.drawLine(20, 50, 80, 50)

    luminance(img.getRGB(50, 50)) should be < 60  // on the line
    luminance(img.getRGB(22, 50)) should be < 60  // just inside the start
    luminance(img.getRGB(18, 50)) should be > 200 // just before the start — a square cap would paint this
    luminance(img.getRGB(82, 50)) should be > 200 // just past the end
  }

  "path geometry freezes when a segment is added — a later transform moves only the pen" in {
    // Cairo and the canvas record segments in the space current at add time; this backend must agree, or a
    // path built before \scale and filled after it lands in a different place per backend
    val (t, img) = page()

    t.setColor(Color("black"))
    t.newPath()
    t.moveTo(20, 20); t.lineTo(40, 20); t.lineTo(40, 40); t.lineTo(20, 40); t.closePath()
    t.scale(2, 2) // set after the segments were added
    t.fillPath(false)

    luminance(img.getRGB(30, 30)) should be < 60  // the square stayed where it was built…
    luminance(img.getRGB(60, 60)) should be > 240 // …not doubled out to (40..80)
  }

  "the stroke pen takes the transform at stroke time" in {
    val (t, img) = page()

    t.setColor(Color("black"))
    t.setLineWidth(2)
    t.gsave()
    t.scale(3, 3)
    t.newPath(); t.moveTo(5, 20); t.lineTo(30, 20) // device: (15,60)–(90,60)
    t.strokePath()
    t.grestore()

    luminance(img.getRGB(50, 60)) should be < 60  // a 6-device-unit pen covers the row
    luminance(img.getRGB(50, 55)) should be > 240 // two points above the 6-wide stroke — clear
  }

  // A full PictureBox replayed through the real backend: a rect drawn near the bottom of the box's own y-up
  // space must land near the bottom of the page, confirming the y-up→device flip points the right way.
  "a PictureBox draws its display list with y up, origin at the bottom" in {
    val (t, img) = page()

    val pm = new PictureMode(t, 100, 100)
    pm.setFill(Color("red"))
    pm.rect(0, 0, 100, 20) // a strip along the bottom edge in picture-local coordinates
    val box = pm.result.asInstanceOf[PictureBox]

    box.draw(t, 0, 100) // baseline at the bottom of a 100-tall page

    red(img.getRGB(50, 90)) should be > 200    // near the page bottom — inside the strip
    green(img.getRGB(50, 90)) should be < 60
    blue(img.getRGB(50, 90)) should be < 60
    luminance(img.getRGB(50, 10)) should be > 240 // near the page top — above the strip, still white
  }
