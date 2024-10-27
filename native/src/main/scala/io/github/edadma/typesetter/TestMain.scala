package io.github.edadma.typesetter

@main def run(): Unit =
  val t = CairoPDFTypesetter("a.pdf")

  t.start()
  t add "asdf"
  t add " "
  t add "zxcv"
  t.paragraph()
  t.fil
  t.end()
  t.destroy()
