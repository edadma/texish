package io.github.edadma.typesetter

@main def run(): Unit =
  val t = CairoPDFTypesetter("a.pdf")

//  t.start()
//  t add "[ppasdf"
//
//  for _ <- 1 to 20 do
//    t add " "
//    t add "[hTz[[["
//
//  t.paragraph()
//  t.fil

  t.halign
    .add("(").op("placeholder").add(")").add(" ").fil
    .op("newColumn").add("[").op("placeholder").add("]").fil
    .op("newLine")
    .add("asdf").op("newColumn").add("zxcv")
    .op("newLine")
    .add("asdf1").op("newColumn").add("zxcv1")

  t.end()
  t.destroy()
