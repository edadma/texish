package io.github.edadma.typesetter

@main def run(): Unit =
  val t = CairoPDFTypesetter("a.pdf")

  t add "asdf"
  t add "zxcv"
  t.paragraph()
  t.fil
  t.end()
  t.destroy()
