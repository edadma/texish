package io.github.edadma.typesetter

@main def run(): Unit =
  val t = CairoPDFTypesetter()

  t.output = "a.pdf"
  t add "asdf"
  t.end()
  t.destroy()
