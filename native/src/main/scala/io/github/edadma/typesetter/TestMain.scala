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

  t.add("asdf")
  t.selectFont("gentium", 14, Set("regular"))
  t.add("asdf")
  t.selectFont("noto", 14, Set("regular"))
  t.add("asdf")

  t.end()
  t.destroy()
