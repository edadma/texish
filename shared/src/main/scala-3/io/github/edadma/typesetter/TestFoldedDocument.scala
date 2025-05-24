package io.github.edadma.typesetter

class TestFoldedDocument extends Document:
  val folds = 2

  def init(): Unit =
    t.set(
      Seq(
        "paperwidth"  -> 400,
        "paperheight" -> 400,
        "pagewidth"   -> 400,
        "pageheight"  -> 400,
        "hsize"       -> 300,
        "vsize"       -> 300,
        "hoffset"     -> 50,
        "voffset"     -> 50,
      ),
    )

  def layout(b: Box): Box = b

  infix def add(box: Box): Unit =
    val fold  = page % folds
    val width = t.getNumber("paperwidth") / folds

    if fold == 0 then
      printedPages += t.createPageTarget
      eject = true

    t.draw(
      layout(box),
      fold * width + t.getNumber("hoffset"),
      t.getNumber("voffset"),
    )

    if fold == folds - 1 then
      t.ejectPageTarget()
      eject = false

    page += 1
