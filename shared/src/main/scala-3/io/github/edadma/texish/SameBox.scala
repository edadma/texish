package io.github.edadma.texish

trait SameBox(box: Box) extends Box:
  def ascent: Double = box.ascent

  def descent: Double = box.descent

  def width: Double = box.width

  def xAdvance: Double = box.xAdvance

  def isSpace: Boolean = box.isSpace
