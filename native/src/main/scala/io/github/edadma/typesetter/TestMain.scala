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

  t add RuleBox(t, 20, 2, 0)

  t.halign
    .add("(").op("placeholder").add(")").add(" ").fil
    .op("newColumn").add("[").op("placeholder").add("]").fil
    .op("newLine")
    .add("asdf").op("newColumn").add("zxcv")
    .op("newLine")
    .add("asdf").add(" ").add("1").op("newColumn").add("zxcv 1")

  t.end()
  t.destroy()
