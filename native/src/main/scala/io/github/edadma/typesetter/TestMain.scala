package io.github.edadma.typesetter

@main def run(): Unit =
  val t = CairoPDFTypesetter()

  t.output = "a.pdf"
  println(0)
//  t add "asdf"
//  println(1)
//  t.end()
//  println(2)
//  t.destroy()
//  println(3)
