package io.github.edadma.texish

/** \raisebox{lift}{content}: the inner box shifted up by `lift` (down when negative), with the box's own height
  * and depth adjusted to match the new position. This is the difference from \raise / \lower, which move where a
  * box draws but leave its stated metrics — and therefore the surrounding line — unchanged.
  */
class RaiseBox(inner: Box, lift: Double) extends SameBox(inner):
  override def ascent: Double  = math.max(0.0, inner.ascent + lift)
  override def descent: Double = math.max(0.0, inner.descent - lift)

  def draw(t: Typesetter, x: Double, y: Double): Unit =
    inner.draw(t, x, y - lift)

  override def toString: String = s"RaiseBox(lift=$lift, inner=$inner)"
