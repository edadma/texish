package io.github.edadma.typesetter

class ZFoldedDocument extends Document:
  val hfolds = 3
  val vfolds = 2

  def init(): Unit =
    val LETTER_WIDTH       = 8.5 * t.in
    val LETTER_HEIGHT      = 11 * t.in
    val LETTER_WIDTH_THIRD = LETTER_WIDTH / 3
    val LETTER_HEIGHT_HALF = LETTER_HEIGHT / 2
    val MARGIN             = 2 * t.mm

    t.set(
      Seq(
        "paperwidth"  -> LETTER_WIDTH,
        "paperheight" -> LETTER_HEIGHT,
//        "pagewidth" -> 400,
//        "pageheight" -> 400,
        "hsize"   -> (LETTER_WIDTH_THIRD - 2 * MARGIN),
        "vsize"   -> (LETTER_HEIGHT_HALF - 2 * MARGIN),
        "hoffset" -> MARGIN,
        "voffset" -> MARGIN,
      ),
    )

  def layout(b: Box): Box = b

  infix def add(box: Box): Unit =
    val folds  = vfolds * hfolds
    val fold   = page % folds
    val hfold  = page % hfolds
    val vfold  = page / hfolds
    val width  = t.getNumber("paperwidth") / hfolds
    val height = t.getNumber("paperheight") / vfolds

    if fold == 0 then
      printedPages += t.createPageTarget(t.getNumber("paperwidth"), t.getNumber("paperheight"))
      eject = true

    t.draw(
      layout(box),
      hfold * width + t.getNumber("hoffset"),
      vfold * height + t.getNumber("voffset"),
    )

    if fold == folds - 1 then
      t.ejectPageTarget()
      eject = false

    page += 1
