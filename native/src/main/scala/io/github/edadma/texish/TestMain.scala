package io.github.edadma.texish

@main def run(): Unit =
  val t1 = new CairoPDFTypesetter("a4.pdf")
  t1.set("paperwidth", 210 * t1.mm)  // A4 width
  t1.set("paperheight", 297 * t1.mm) // A4 height
  t1.add("This is A4 size")
  t1.end()
  t1.destroy()

  // Test Letter size
  val t2 = new CairoPDFTypesetter("letter.pdf")
  t2.set("paperwidth", 8.5 * t2.in)
  t2.set("paperheight", 11 * t2.in)
  t2.add("This is Letter size")
  t2.end()
  t2.destroy()
