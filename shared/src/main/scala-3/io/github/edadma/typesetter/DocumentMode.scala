package io.github.edadma.typesetter

import scala.collection.mutable.ArrayBuffer
import scala.compiletime.uninitialized

class DocumentMode(val t: Typesetter) extends Mode:
  val printedPages   = new ArrayBuffer[Any]
  var page: Int      = 0
  var eject: Boolean = false

  def init(): Unit = ()

  def layout(b: Box): Box = b

  infix def add(box: Box): Unit =
    t.get("layout") match
      case "zfold" => handleZFoldLayout(box)
      case _       => handleSimpleLayout(box)

    page += 1

  def handleZFoldLayout(b: Box): Unit =
    val hfolds = 3
    val vfolds = 2

    val folds = vfolds * hfolds

    val fold   = page % folds
    val hfold  = page % hfolds
    val vfold  = page / hfolds
    val width  = t.getNumber("paperwidth") / hfolds
    val height = t.getNumber("paperheight") / vfolds

    if fold == 0 then
      printedPages += t.createPageTarget
      eject = true

    t.draw(
      layout(b),
      hfold * width + t.getNumber("hoffset"),
      vfold * height + t.getNumber("voffset"),
    )

    if fold == folds - 1 then
      t.ejectPageTarget()
      eject = false

  def handleSimpleLayout(b: Box): Unit =
    printedPages += t.createPageTarget
    t.draw(
      layout(b),
      t.getNumber("hoffset"),
      t.getNumber("voffset"),
    )
    t.ejectPageTarget()

  override def done(): Unit =
    pop

    if eject then
      t.ejectPageTarget()

  def result: Box = ???
