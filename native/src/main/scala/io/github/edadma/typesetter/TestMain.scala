package io.github.edadma.typesetter

@main def run(): Unit =
  val t = CairoPDFTypesetter("a.pdf")

  t.start()
  t add "[ppasdf"

  for _ <- 1 to 20 do
    t add " "
    t add "[hTz[[["

  t.paragraph()
  t.fil
  t.end()
  t.destroy()
