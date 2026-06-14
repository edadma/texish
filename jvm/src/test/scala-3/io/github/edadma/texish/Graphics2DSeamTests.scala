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
