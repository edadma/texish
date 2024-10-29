package io.github.edadma.typesetter

trait Mode:
  def t: Typesetter

  def init(): Unit

  infix def add(box: Box): Unit

  def result: Box

  def op(operation: String): Unit = sys.error(s"illegal operation '$operation'")

  def done(): Unit =
    pop
    t.add(result)

  def pop: Mode = t.modeStack.pop

  def top: Mode = t.mode
