package io.github.edadma.texish.parser

import io.github.edadma.char_reader.CharReader
import io.github.edadma.texish.{HeadlessTypesetter, TexishException}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class ProcessorTests extends AnyFreeSpec with Matchers:

  def process(input: String): String =
    val handler = new StringHandler
    val proc = new Processor(handler)
    proc.process(input)
    handler.result

  // A processor with the full typesetting primitives, so the active characters (#, &, ^, _) are
  // registered the way a real document sees them.
  private def typesetProc(): Processor =
    val t       = new HeadlessTypesetter
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)
    proc

  "Processor" - {
    "should pass through plain text" in {
      process("hello world") shouldBe "hello world"
    }

    "should handle groups for scoping" in {
      process("{hello}") shouldBe "hello"
    }

    "should handle nested groups" in {
      process("a{b{c}d}e") shouldBe "abcde"
    }

    "should define and expand macros" in {
      process("\\def foo {bar}\\foo") shouldBe "bar"
    }

    "a value keeps a literal active character: an all-digit hex colour is not collapsed to a dimension" in {
      // # is an active character (the \halign placeholder) but stands for itself elsewhere; a value like
      // a #RRGGBB colour must keep it. Before the fix, {#808080} dropped the # and read 808080 as a
      // dimension (\"808080pt\"), while {#b0b0b0} survived only because its letters blocked the number.
      val proc = typesetProc()
      proc.process("\\set c {#808080}\\set d {#b0b0b0}")
      proc.handler.get("c") shouldBe Value.Text("#808080")
      proc.handler.get("d") shouldBe Value.Text("#b0b0b0")
    }

    "should handle macro with content after" in {
      process("\\def x {X}a\\x b") shouldBe "aX b"
    }

    "\\ignorespaces swallows the spaces that follow it" in {
      process("a\\ignorespaces   b") shouldBe "ab"
    }

    "a macro ending in \\ignorespaces eats the gap before its trailing text" in {
      process("\\def item {*\\ignorespaces}\\item the rest") shouldBe "*the rest"
    }

    "should handle \\set and \\get with braces" in {
      process("\\set x {hello}\\get\\x") shouldBe "hello"
    }

    "should handle \\set without braces" in {
      process("\\set x 42\\the\\x") shouldBe "42"
    }

    "should handle \\the" in {
      process("\\set x world\\the\\x") shouldBe "world"
    }

    "should handle \\if with true condition" in {
      process("\\if{1}yes\\fi") shouldBe "yes"
    }

    "should handle \\if with false condition" in {
      process("\\if{0}yes\\fi") shouldBe ""
    }

    "should handle \\if\\else with true" in {
      process("\\if{1}yes\\else no\\fi") shouldBe "yes"
    }

    "should handle \\if\\else with false" in {
      process("\\if{0}yes\\else no\\fi") shouldBe "no"
    }

    "should handle ~ as non-breaking space" in {
      process("a~b") shouldBe "a\u00A0b"
    }

    "should handle parameterized macro with one param" in {
      process("\\def greet name {Hello, \\name!}\\greet{World}") shouldBe "Hello, World!"
    }

    "should handle parameterized macro with two params" in {
      process("\\def full first last {\\first \\last}\\full{John}{Doe}") shouldBe "John Doe"
    }

    "should handle parameterized macro with nested braces in arg" in {
      process("\\def wrap x {[\\x]}\\wrap{{nested}}") shouldBe "[nested]"
    }

    "should handle macro param used multiple times" in {
      process("\\def double x {\\x\\x}\\double{ab}") shouldBe "abab"
    }

    "should handle macro with no params (backward compat)" in {
      process("\\def foo {bar}\\foo") shouldBe "bar"
    }

    // ============ ARITHMETIC TESTS ============

    "should handle \\+ with numbers" in {
      process("\\+{3}{4}") shouldBe "7"
    }

    "should handle \\+ with decimals" in {
      process("\\+{1.5}{2.5}") shouldBe "4"
    }

    "should handle \\+ for string concatenation" in {
      process("\\+{hello}{world}") shouldBe "helloworld"
    }

    "should handle \\- with numbers" in {
      process("\\-{10}{3}") shouldBe "7"
    }

    "should handle \\- with negative result" in {
      process("\\-{3}{10}") shouldBe "-7"
    }

    "should handle \\* with numbers" in {
      process("\\*{6}{7}") shouldBe "42"
    }

    "should handle \\* with decimals" in {
      process("\\*{2.5}{4}") shouldBe "10"
    }

    "should handle \\/ with numbers" in {
      process("\\/{20}{4}") shouldBe "5"
    }

    "should handle \\/ with decimals" in {
      process("\\/{7}{2}") shouldBe "3.5"
    }

    // ============ COMPARISON TESTS ============

    "should handle \\= with equal numbers" in {
      process("\\={5}{5}") shouldBe "true"
    }

    "should handle \\= with unequal numbers" in {
      process("\\={5}{6}") shouldBe "false"
    }

    "should handle \\= with equal strings" in {
      process("\\={hello}{hello}") shouldBe "true"
    }

    "should handle \\= with unequal strings" in {
      process("\\={hello}{world}") shouldBe "false"
    }

    "should handle \\< with numbers" in {
      process("\\<{3}{5}") shouldBe "true"
    }

    "should handle \\< when not less" in {
      process("\\<{5}{3}") shouldBe "false"
    }

    "should handle \\> with numbers" in {
      process("\\>{7}{2}") shouldBe "true"
    }

    "should handle \\> when not greater" in {
      process("\\>{2}{7}") shouldBe "false"
    }

    "should handle \\<= with less" in {
      process("\\<={3}{5}") shouldBe "true"
    }

    "should handle \\<= with equal" in {
      process("\\<={5}{5}") shouldBe "true"
    }

    "should handle \\<= when greater" in {
      process("\\<={7}{5}") shouldBe "false"
    }

    "should handle \\>= with greater" in {
      process("\\>={7}{5}") shouldBe "true"
    }

    "should handle \\>= with equal" in {
      process("\\>={5}{5}") shouldBe "true"
    }

    "should handle \\>= when less" in {
      process("\\>={3}{5}") shouldBe "false"
    }

    "should handle \\!= with different values" in {
      process("\\!={5}{6}") shouldBe "true"
    }

    "should handle \\!= with same values" in {
      process("\\!={5}{5}") shouldBe "false"
    }

    // Every comparison sets a capturable Bool result, like \=, so a comparison can be stored in a variable and
    // composed in further expressions — not just typeset as the text "true"/"false".
    "comparisons set a capturable Bool result" in {
      val handler = new StringHandler
      val proc    = new Processor(handler)
      proc.process("\\set a {\\<{3}{5}}\\set b {\\>{3}{5}}\\set c {\\<={5}{5}}\\set d {\\>={3}{5}}\\set e {\\!={1}{1}}")
      handler.get("a") shouldBe Value.Bool(true)
      handler.get("b") shouldBe Value.Bool(false)
      handler.get("c") shouldBe Value.Bool(true)
      handler.get("d") shouldBe Value.Bool(false)
      handler.get("e") shouldBe Value.Bool(false)
    }

    // An ordering comparison between a number and a numeric string compares them numerically rather than failing
    // — the operand types differ when one came from a sequence element (text-typed) and the other is computed.
    // (Text vs text stays lexical: "3" > "20".)
    "ordering compares a number against a numeric string numerically" in {
      // \cat forces a Text operand, \calc a Num operand: the mixed pair orders numerically.
      process("\\set t {\\cat{3}{}}\\<{\\t}{\\calc{20}}") shouldBe "true"
      process("\\set t {\\cat{3}{}}\\>{\\t}{\\calc{20}}") shouldBe "false"
      // Two text operands still compare lexically: "3" > "20".
      process("\\<{\\cat{3}{}}{\\cat{20}{}}") shouldBe "false"
    }

    "\\round trims floating-point noise to a clean decimal label" in {
      process("\\round{\\calc{0.1 + 0.2}}{2}") shouldBe "0.3"
    }

    "\\round to zero places gives a whole number with no decimal point" in {
      process("\\round{2.7}{0}") shouldBe "3"
    }

    "\\round keeps the significant decimals and drops trailing zeros" in {
      process("\\round{3.14159}{3}") shouldBe "3.142"
      process("\\round{10}{2}") shouldBe "10"
    }

    "\\round sets a capturable number result" in {
      val handler = new StringHandler
      val proc    = new Processor(handler)
      proc.process("\\set v {\\round{\\calc{7/3}}{2}}")
      handler.get("v") shouldBe Value.Num(2.33)
    }

    // A variable holding a numeric string coerces to a number where arithmetic expects one — \calc and \round
    // both resolve it rather than failing with "unknown name". \seq can yield text-typed numeric elements (its
    // boundary items, after macro-parameter substitution), so this coercion is what lets a data series built from
    // such a sequence be used in coordinate arithmetic.
    "a numeric string variable is usable as a number in \\calc and \\round" in {
      // \cat yields a Text value, so s holds the numeric string "5", not a Num.
      process("\\set s {\\cat{5}{}}\\calc{s * 2}") shouldBe "10"
      process("\\set s {\\cat{2.4}{}}\\round{\\s}{0}") shouldBe "2"
    }

    // ============ STRING FUNCTION TESTS ============

    "should handle \\upcase" in {
      process("\\upcase{hello}") shouldBe "HELLO"
    }

    "should handle \\upcase with mixed case" in {
      process("\\upcase{Hello World}") shouldBe "HELLO WORLD"
    }

    "should handle \\downcase" in {
      process("\\downcase{HELLO}") shouldBe "hello"
    }

    "should handle \\downcase with mixed case" in {
      process("\\downcase{Hello World}") shouldBe "hello world"
    }

    "should handle \\trim" in {
      process("\\trim{  hello  }") shouldBe "hello"
    }

    "should handle \\trim with tabs" in {
      process("\\trim{\thello\t}") shouldBe "hello"
    }

    "should handle \\size with string" in {
      process("\\size{hello}") shouldBe "5"
    }

    "should handle \\size with empty string" in {
      process("\\size{}") shouldBe "0"
    }

    // ============ SEQUENCE FUNCTION TESTS ============

    "should handle \\seq creating a sequence" in {
      process("\\set s {\\seq{a b c}}\\the\\s") shouldBe "[a, b, c]"
    }

    "should not add a phantom element for a macro argument with a trailing space" in {
      // a macro argument is brace-wrapped on substitution, so \seq{\x} becomes \seq{{a b c }}; stripping every
      // wrapping layer keeps the trailing space from splitting off the closing brace as an empty element
      process("\\def f x {\\set s {\\seq{\\x}}\\the\\s}\\f{a b c}") shouldBe "[a, b, c]"
      process("\\def f x {\\set s {\\seq{\\x}}\\the\\s}\\f{a b c }") shouldBe "[a, b, c]"
    }

    "should split a doubly-wrapped sequence like a singly-wrapped one" in {
      // the extra brace layer (as macro substitution produces) is stripped, not split into empty elements
      process("\\set s {\\seq{{a b c}}}\\the\\s") shouldBe "[a, b, c]"
    }

    "should handle \\head with string" in {
      process("\\head{hello}") shouldBe "h"
    }

    "should handle \\last with string" in {
      process("\\last{hello}") shouldBe "o"
    }

    // ============ FOR LOOP TESTS ============

    "should handle \\for loop with inline range" in {
      process("\\for\\i{\\range{1}{3}}{\\the\\i }") shouldBe "1 2 3 "
    }

    "should handle \\for loop with forloop.index" in {
      process("\\for\\x{\\range{1}{2}}{\\forloop.index }") shouldBe "1 2 "
    }

    "should handle \\for loop with forloop.first" in {
      process("\\for\\x{\\range{1}{3}}{\\if{\\forloop.first}FIRST\\fi}") shouldBe "FIRST"
    }

    "should handle \\for loop with forloop.last" in {
      process("\\for\\x{\\range{1}{3}}{\\if{\\forloop.last}LAST\\fi}") shouldBe "LAST"
    }

    "should handle \\for loop with seq" in {
      process("\\for\\item{\\seq{apple banana cherry}}{\\the\\item,}") shouldBe "apple,banana,cherry,"
    }

    "should handle \\for loop with forloop.length" in {
      process("\\for\\x{\\range{1}{5}}{\\forloop.length}") shouldBe "55555"
    }

    "should handle nested arithmetic in condition" in {
      process("\\if{\\+{1}{0}}yes\\fi") shouldBe "yes"
    }

    "should handle comparison in if condition" in {
      process("\\if{\\>{5}{3}}bigger\\fi") shouldBe "bigger"
    }

    // ============ EXPRESSION EVALUATION TESTS ============

    "should handle \\set with expression value" in {
      process("\\set x {\\+{2}{3}}\\the\\x") shouldBe "5"
    }

    "should handle \\set with sequence expression" in {
      process("\\set nums {\\range{1}{3}}\\the\\nums") shouldBe "[1, 2, 3]"
    }

    "should handle \\if with variable condition" in {
      process("\\set flag {1}\\if{\\flag}yes\\fi") shouldBe "yes"
    }

    "should handle \\if with false variable" in {
      process("\\set flag {0}\\if{\\flag}yes\\else no\\fi") shouldBe "no"
    }

    // ============ FORLOOP METADATA TESTS ============

    "should handle forloop.indexz (zero-based)" in {
      process("\\for\\x{\\range{1}{3}}{\\forloop.indexz,}") shouldBe "0,1,2,"
    }

    "should handle forloop.rindex (reverse index)" in {
      process("\\for\\x{\\range{1}{3}}{\\forloop.rindex,}") shouldBe "3,2,1,"
    }

    "should handle forloop.element" in {
      process("\\for\\x{\\seq{a b c}}{\\forloop.element}") shouldBe "abc"
    }

    "should handle multiple forloop properties in one loop" in {
      process("\\for\\x{\\range{1}{2}}{[\\forloop.index/\\forloop.length]}") shouldBe "[1/2][2/2]"
    }

    // ============ NESTED LOOPS AND EXPRESSIONS ============

    "should handle nested for loops" in {
      process("\\for\\i{\\range{1}{2}}{\\for\\j{\\range{1}{2}}{(\\the\\i,\\the\\j)}}") shouldBe "(1,1)(1,2)(2,1)(2,2)"
    }

    "should handle arithmetic in for loop body" in {
      process("\\for\\i{\\range{1}{3}}{\\*{\\i}{2},}") shouldBe "2,4,6,"
    }

    "should handle conditional in for loop with forloop" in {
      process("\\for\\x{\\range{1}{3}}{\\if{\\forloop.first}F\\else M\\fi}") shouldBe "FMM"
    }

    // ============ SEQUENCE EDGE CASES ============

    "should handle empty sequence in for loop" in {
      process("\\for\\x{\\seq{}}{X}") shouldBe ""
    }

    "should handle single item sequence" in {
      process("\\for\\x{\\seq{only}}{\\the\\x}") shouldBe "only"
    }

    "should handle forloop.first and forloop.last for single item" in {
      process("\\for\\x{\\seq{one}}{\\if{\\forloop.first}F\\fi\\if{\\forloop.last}L\\fi}") shouldBe "FL"
    }

    // ============ RANGE EDGE CASES ============

    "should handle range with same start and end" in {
      process("\\for\\i{\\range{5}{5}}{\\the\\i}") shouldBe "5"
    }

    "should handle range used directly in expression" in {
      process("\\size{\\range{1}{10}}") shouldBe "10"
    }

    // ============ FILE INCLUSION TESTS ============

    "should handle \\include" in {
      process("\\include{shared/src/test/resources/test-include.parser}\\greeting") shouldBe "Hello from included file!"
    }

    // ============ TEMPLATE TESTS ============

    "Template.render should substitute variables" in {
      val result = Template.render("Hello, \\the\\name!", Map("name" -> Value.Text("World")))
      result shouldBe "Hello, World!"
    }

    "Template.render should handle loops with data" in {
      val data = Map("items" -> Value.Seq(Vector(Value.Text("a"), Value.Text("b"), Value.Text("c"))))
      val result = Template.render("\\for\\i{\\items}{\\the\\i,}", data)
      result shouldBe "a,b,c,"
    }

    "Template.render should handle conditionals with data" in {
      val data = Map("show" -> Value.Bool(true))
      val result = Template.render("\\if{\\show}visible\\fi", data)
      result shouldBe "visible"
    }

    "Template.render should handle nested map data" in {
      val data = Map("user" -> Value.Map(Map("name" -> Value.Text("Alice"), "age" -> Value.Num(30))))
      val result = Template.render("\\user.name is \\user.age", data)
      result shouldBe "Alice is 30"
    }

    // ============ MAP CREATION TESTS ============

    "should create map with \\map" in {
      process("\\set m {\\map{name Alice age 30}}\\m.name") shouldBe "Alice"
    }

    "should access map values with dot notation" in {
      process("\\set m {\\map{x 1 y 2}}\\m.x and \\m.y") shouldBe "1 and 2"
    }

    "should iterate over map entries" in {
      process("\\set m {\\map{a 1 b 2}}\\for\\e{\\m}{\\e.key=\\e.value,}") shouldBe "a=1,b=2,"
    }

    "should build a map from a macro argument with a trailing space" in {
      // the same brace-wrapping that broke \seq would leave \map with an odd, trailing-empty element list
      process("\\def f x {\\set m {\\map{\\x}}\\m.a}\\f{a 1 b 2 }") shouldBe "1"
    }

    // ============ ESCAPE SEQUENCE TESTS ============

    "should escape { with \\{" in {
      process("\\{") shouldBe "{"
    }

    "should escape } with \\}" in {
      process("\\}") shouldBe "}"
    }

    "should escape % with \\%" in {
      process("\\%") shouldBe "%"
    }

    "should escape \\ with \\\\" in {
      process("\\\\") shouldBe "\\"
    }

    "should escape ~ with \\~" in {
      process("\\~") shouldBe "~"
    }

    "should treat # as normal text" in {
      process("a#b") shouldBe "a#b"
    }

    // ============ ACTIVE CHARACTER TESTS ============

    "should handle registered active character" in {
      val handler = new StringHandler
      val proc = new Processor(handler)
      proc.registerActive('#', new Active {
        def execute(proc: Processor, c: Char, pos: CharReader): Unit =
          proc.handler.text("[HASH]")
      })
      proc.process("a#b")
      handler.result shouldBe "a[HASH]b"
    }

    "should handle multiple registered active characters" in {
      val handler = new StringHandler
      val proc = new Processor(handler)
      proc.registerActive('#', new Active {
        def execute(proc: Processor, c: Char, pos: CharReader): Unit =
          proc.handler.text("[HASH]")
      })
      proc.registerActive('&', new Active {
        def execute(proc: Processor, c: Char, pos: CharReader): Unit =
          proc.handler.text("[AMP]")
      })
      proc.process("a#b&c")
      handler.result shouldBe "a[HASH]b[AMP]c"
    }

    "should still handle ~ as non-breaking space when no active registered" in {
      process("a~b") shouldBe "a\u00A0b"
    }

    "should allow overriding ~ with registered active" in {
      val handler = new StringHandler
      val proc = new Processor(handler)
      proc.registerActive('~', new Active {
        def execute(proc: Processor, c: Char, pos: CharReader): Unit =
          proc.handler.text("[TILDE]")
      })
      proc.process("a~b")
      handler.result shouldBe "a[TILDE]b"
    }

    "active handler should have access to processor for reading arguments" in {
      val handler = new StringHandler
      val proc = new Processor(handler)
      proc.registerActive('@', new Active {
        def execute(proc: Processor, c: Char, pos: CharReader): Unit =
          // Read the next argument
          val arg = proc.readArgument(pos)
          val text = arg.map {
            case Token.Text(s, _) => s
            case _ => ""
          }.mkString
          proc.handler.text(s"[@:$text]")
      })
      proc.process("a@{hello}b")
      handler.result shouldBe "a[@:hello]b"
    }

    // ============ OPTIONAL PARAMETER TESTS ============

    "should parse single optional parameter with numeric value" in {
      val handler = new StringHandler
      val proc = new Processor(handler)
      proc.registerPrimitive("test", new Primitive {
        def execute(proc: Processor, pos: CharReader): Unit =
          val opts = proc.readOptionalParams(pos)
          proc.handler.text(s"to=${opts.get("to").map(Value.display).getOrElse("none")}")
      })
      proc.process("\\test to:100")
      handler.result shouldBe "to=100"
    }

    "should parse multiple optional parameters" in {
      val handler = new StringHandler
      val proc = new Processor(handler)
      proc.registerPrimitive("test", new Primitive {
        def execute(proc: Processor, pos: CharReader): Unit =
          val opts = proc.readOptionalParams(pos)
          val w = opts.get("width").map(Value.display).getOrElse("?")
          val h = opts.get("height").map(Value.display).getOrElse("?")
          proc.handler.text(s"w=$w,h=$h")
      })
      proc.process("\\test width:50 height:30")
      handler.result shouldBe "w=50,h=30"
    }

    "should parse optional parameter with text value" in {
      val handler = new StringHandler
      val proc = new Processor(handler)
      proc.registerPrimitive("test", new Primitive {
        def execute(proc: Processor, pos: CharReader): Unit =
          val opts = proc.readOptionalParams(pos)
          proc.handler.text(s"name=${opts.get("name").map(Value.display).getOrElse("none")}")
      })
      proc.process("\\test name:hello")
      handler.result shouldBe "name=hello"
    }

    "should parse optional parameter followed by regular argument" in {
      val handler = new StringHandler
      val proc = new Processor(handler)
      proc.registerPrimitive("test", new Primitive {
        def execute(proc: Processor, pos: CharReader): Unit =
          val opts = proc.readOptionalParams(pos)
          val arg = proc.readArgument(pos)
          val argText = arg.map {
            case Token.Text(s, _) => s
            case _ => ""
          }.mkString
          val to = opts.get("to").map(Value.display).getOrElse("none")
          proc.handler.text(s"to=$to,arg=$argText")
      })
      proc.process("\\test to:100 {content}")
      handler.result shouldBe "to=100,arg=content"
    }

    "should handle no optional parameters" in {
      val handler = new StringHandler
      val proc = new Processor(handler)
      proc.registerPrimitive("test", new Primitive {
        def execute(proc: Processor, pos: CharReader): Unit =
          val opts = proc.readOptionalParams(pos)
          proc.handler.text(s"count=${opts.size}")
      })
      proc.process("\\test {arg}")
      // {arg} is processed after primitive returns, braces create scope, "arg" is output
      handler.result shouldBe "count=0arg"
    }

    "should parse optional parameter with variable reference" in {
      val handler = new StringHandler
      val proc = new Processor(handler)
      proc.registerPrimitive("test", new Primitive {
        def execute(proc: Processor, pos: CharReader): Unit =
          val opts = proc.readOptionalParams(pos)
          proc.handler.text(s"to=${opts.get("to").map(Value.display).getOrElse("none")}")
      })
      proc.process("\\set size 200\\test to:\\size")
      handler.result shouldBe "to=200"
    }

    // ============ TRAILING WHITESPACE CONSUMPTION TESTS ============

    "should consume trailing whitespace after non-braced argument" in {
      // Space after "42" is consumed, so "hello" has no leading space
      process("\\set x 42 hello") shouldBe "hello"
    }

    "should consume trailing whitespace after macro with non-braced argument" in {
      // Space after "World" is consumed (non-braced arg), so output is "Hi Worldend"
      process("\\def greet name {Hi \\name}\\greet World end") shouldBe "Hi Worldend"
    }

    "should not add extra space before text following command with non-braced args" in {
      process("\\set a 1 \\set b 2 done") shouldBe "done"
    }

    "should preserve space after braced args" in {
      // Space after "}" is NOT consumed (braced args don't consume trailing space)
      // Matches the reference implementation
      process("\\set x {42} hello") shouldBe " hello"
    }

    // ============ ACCENTS ============

    "should handle \\accent with acute" in {
      process("caf\\accent ' e") shouldBe "café"
    }

    "should handle \\accent with grave" in {
      process("\\accent ` a") shouldBe "à"
    }

    "should handle \\accent with circumflex" in {
      process("\\accent ^ e") shouldBe "ê"
    }

    "should handle \\accent with quoted umlaut mark" in {
      process("\\accent '\"' u") shouldBe "ü"
    }

    "should handle \\accent with quoted apostrophe mark" in {
      process("\\accent \"'\" E") shouldBe "É"
    }

    "should handle \\accent with cedilla" in {
      process("\\accent c c") shouldBe "ç"
    }

    "should handle \\accent with tilde" in {
      // bare ~ is the non-breaking space, so the tilde mark must be braced
      process("\\accent {~} n") shouldBe "ñ"
    }

    "should handle \\accent through a macro" in {
      process("\\def umlaut letter {\\accent '\"' \\letter}fr\\umlaut{u}her") shouldBe "früher"
    }

    "should error on unknown accent combination" in {
      a[TexishException] should be thrownBy process("\\accent c x")
    }
  }
