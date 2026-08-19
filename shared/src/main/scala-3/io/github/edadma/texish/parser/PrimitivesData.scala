package io.github.edadma.texish.parser

import io.github.edadma.char_reader.CharReader

/** The data-handling half of the document language: indexing, slicing, ordering, searching and folding over
  * sequences, strings and maps.
  *
  * texish computes numbers well — `\calc` is a full infix grammar with forty functions — but until these existed a
  * package that needed to *hold* data rather than a number had to emulate it. `packages/plot.texish` is the worked
  * example: it reached a list's second element with `\head{\tail{…}}`, walked a flat `x y x y` list by tail
  * recursion because nothing could pair it up, kept its series in index-keyed maps because sequences could not be
  * indexed, and computed its axis bounds and its least-squares sums by four and five `\global` accumulators, with a
  * comment explaining that the `\global` was needed because `\for` opens a scope per iteration. Every one of those
  * is a primitive here.
  *
  * Three conventions run through the file:
  *
  *   - **A string is a sequence of its characters**, so `\nth`, `\slice`, `\reverse`, `\contains` and `\indexof`
  *     take either and give back the kind they were given. Characters means *code points*, not UTF-16 units, so an
  *     emoji or a math alphanumeric is one item rather than two broken surrogate halves — the same rule `\head`,
  *     `\tail`, `\last` and `\for` already follow.
  *   - **Positions are 1-based and inclusive**, as they are everywhere a document counts things (lines, pages,
  *     items). `\indexof` answers 0 for "not found", which is therefore falsy and reads as `\if {\indexof …}`.
  *   - **Ordering is numeric where both sides are numbers and lexicographic otherwise**, so `\sort` on a list of
  *     numbers gives 2, 10 rather than 10, 2, and on a list of words gives alphabetical order.
  */

/** The items of a value, as `\for` sees them: a sequence is its elements, a string its code points, a map its
  * entries as `{key, value}` maps, and nothing else has items. Anything else is a single item, so a scalar behaves
  * like a one-element list rather than an error.
  */
private def itemsOf(v: Value): Vector[Value] = v match
  case Value.Seq(items)            => items
  case Value.Text(s)               => codePointStrings(s).map(Value.Text.apply)
  case Value.Map(entries)          => entries.map((k, x) => Value.Map(Map("key" -> Value.Text(k), "value" -> x))).toVector
  case Value.Nil | Value.Undefined => Vector.empty
  case other                       => Vector(other)

/** Rebuild a result in the shape of the value it came from: an operation on a string gives a string, an operation
  * on anything else a sequence. This is what lets `\reverse` and `\slice` serve both without a second name each.
  */
private def likeInput(original: Value, items: Vector[Value]): Value = original match
  case Value.Text(_) => Value.Text(items.map(Value.display).mkString)
  case _             => Value.Seq(items)

/** Compare two values for ordering: numerically when both read as numbers, otherwise by their display text. */
private def compareValues(a: Value, b: Value): Int =
  (Value.number(a), Value.number(b)) match
    case (Some(x), Some(y)) => java.lang.Double.compare(x, y)
    case _                  => Value.display(a).compareTo(Value.display(b))

/** Whether two values count as the same item for `\contains` / `\indexof`: their display text, so the number 3 and
  * the text "3" match, as they already do for a map key.
  */
private def sameItem(a: Value, b: Value): Boolean = Value.display(a) == Value.display(b)


// ============ INDEXING AND SLICING ============

/** `\nth {items} {n}` — the nth item of a sequence, or the nth character of a string, counting from 1.
  *
  * Out of range is Undefined rather than an error, so a lookup past the end tests false instead of stopping the
  * document — the same answer `\mapget` gives for a key that is not there. It must not be Nil: the expression
  * evaluator reads a Nil result as "this produced no value" and falls back to the argument's source text, so a
  * `\nth` past the end would evaluate to the characters of its own arguments.
  */
object NthPrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    val subject = proc.evalStringArgument(pos)
    val n       = Value.number(proc.evalArgumentExpr(pos)).map(_.toInt).getOrElse(0)
    val items   = itemsOf(subject)
    val result  = if n >= 1 && n <= items.length then items(n - 1) else Value.Undefined

    valueResult(proc, result)

/** `\slice {items} {from} {count}` — `count` items (or characters) starting at position `from`, counting from 1.
  * Both bounds are clamped, so a slice that runs off either end gives what is there rather than an error, and a
  * `count` of zero or less gives nothing. A string in gives a string out.
  */
object SlicePrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    val subject = proc.evalStringArgument(pos)
    val from    = Value.number(proc.evalArgumentExpr(pos)).map(_.toInt).getOrElse(1)
    val count   = Value.number(proc.evalArgumentExpr(pos)).map(_.toInt).getOrElse(0)
    val items   = itemsOf(subject)
    val start   = math.max(0, from - 1)
    val taken   = if count <= 0 then Vector.empty else items.drop(start).take(count)

    valueResult(proc, likeInput(subject, taken))

/** `\reverse {items}` — the sequence or string in the opposite order. */
object ReversePrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    val subject = proc.evalStringArgument(pos)

    valueResult(proc, likeInput(subject, itemsOf(subject).reverse))

// ============ BUILDING SEQUENCES ============

/** `\append {seq} {item}` — the sequence with one more item on the end. Sequences are values, not containers that
  * are mutated, so this returns a new one: `\set xs {\append{\xs}{5}}` is how a list grows, and inside a `\for` the
  * assignment needs `\global` to outlive the iteration's scope.
  */
object AppendPrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    val seq  = proc.evalArgumentExpr(pos)
    val item = proc.evalStringArgument(pos)

    valueResult(proc, Value.Seq(itemsOf(seq) :+ item))

/** `\prepend {seq} {item}` — the sequence with one more item on the front. */
object PrependPrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    val seq  = proc.evalArgumentExpr(pos)
    val item = proc.evalStringArgument(pos)

    valueResult(proc, Value.Seq(item +: itemsOf(seq)))

/** `\concat {a} {b}` — one sequence followed by another. This is the sequence counterpart of `\cat`, which joins two
  * values as *text*; the two are deliberately separate, because appending a list to a list and printing a list after
  * a list are different operations and picking one silently would be a trap.
  */
object ConcatPrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    val a = proc.evalArgumentExpr(pos)
    val b = proc.evalArgumentExpr(pos)

    valueResult(proc, Value.Seq(itemsOf(a) ++ itemsOf(b)))

/** `\join {seq} {separator}` — the items as one string with the separator between them. The inverse of `\split`. */
object JoinPrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    val seq = proc.evalArgumentExpr(pos)
    val sep = Value.display(proc.evalStringArgument(pos))

    valueResult(proc, Value.Text(itemsOf(seq).map(Value.display).mkString(sep)))

/** `\chunk {seq} {n}` — the items grouped into sub-sequences of `n`, the last one short if the count does not
  * divide. This is what turns a flat data list into records: a plot's `x y x y …` becomes a list of pairs, which
  * `\for` can then walk one point at a time instead of by two-at-a-time tail recursion.
  */
object ChunkPrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    val seq = proc.evalArgumentExpr(pos)
    val n   = Value.number(proc.evalArgumentExpr(pos)).map(_.toInt).getOrElse(1)

    if n < 1 then proc.handler.error(s"\\chunk: the group size must be at least 1, got $n", pos)

    valueResult(proc, Value.Seq(itemsOf(seq).grouped(n).map(g => Value.Seq(g)).toVector))

// ============ SEARCHING ============

/** `\contains {items} {item}` — whether a sequence holds the item, or a string holds the substring. A Bool, so it
  * reads directly as an `\if` condition.
  */
object ContainsPrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    val subject = proc.evalStringArgument(pos)
    val needle  = proc.evalStringArgument(pos)
    val found = subject match
      case Value.Text(s) => s.contains(Value.display(needle))
      case other         => itemsOf(other).exists(sameItem(_, needle))

    valueResult(proc, Value.Bool(found))

/** `\indexof {items} {item}` — where the item first occurs, counting from 1, or 0 when it does not occur. Zero is
  * falsy, so `\if {\indexof{\xs}{q}}` tests presence and the same call gives the position when it is wanted. A
  * string is searched for a substring, and the position counts characters, matching `\nth` and `\slice`.
  */
object IndexOfPrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    val subject = proc.evalStringArgument(pos)
    val needle  = proc.evalStringArgument(pos)
    val index = subject match
      case Value.Text(s) =>
        // in code points, so the answer indexes the same units \nth and \slice count
        val at = s.indexOf(Value.display(needle))
        if at < 0 then 0 else codePointStrings(s.substring(0, at)).length + 1
      case other => itemsOf(other).indexWhere(sameItem(_, needle)) + 1

    valueResult(proc, Value.Num(index))

// ============ AGGREGATES ============

/** `\total {seq}` — the sum of a sequence of numbers. An item that is not a number is an error naming it, rather
  * than a silent zero that would show up much later as a wrong total.
  *
  * Not `\sum`: that name belongs to the math-mode big operator, and taking it here silently broke every display
  * containing a summation sign. A data primitive and a typesetting primitive share one namespace, so a new name
  * has to be checked against the math symbols as well as against the primitive table.
  */
object TotalPrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    val seq = proc.evalArgumentExpr(pos)
    val total = itemsOf(seq).foldLeft(0.0) { (acc, item) =>
      acc + Value.number(item).getOrElse(proc.handler.error(s"\\total: '${Value.display(item)}' is not a number", pos))
    }

    valueResult(proc, Value.Num(total))

/** `\minimum {seq}` / `\maximum {seq}` — the least and greatest item, by the same ordering `\sort` uses, so they
  * work on words as well as numbers. An empty sequence gives Undefined, for the reason `\nth` does.
  */
private class ExtremePrimitive(name: String, keepLeft: (Int) => Boolean) extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    val items = itemsOf(proc.evalArgumentExpr(pos))
    val result =
      if items.isEmpty then Value.Undefined
      else items.reduceLeft((a, b) => if keepLeft(compareValues(a, b)) then a else b)

    valueResult(proc, result)

object MinimumPrimitive extends ExtremePrimitive("minimum", _ <= 0)
object MaximumPrimitive extends ExtremePrimitive("maximum", _ >= 0)

// ============ ORDERING AND FOLDING WITH A BODY ============

/** The shared shape of `\filter`, `\transform` and `\sortby`: `\command \var {sequence} {expression}`. Each binds
  * `\var` to one item at a time and evaluates the expression for it, exactly as `\for` binds its loop variable —
  * the difference is that these collect the expression's *value* instead of typesetting its output, which is what
  * `\for` cannot do and why a package that needs a result has had to accumulate through a `\global` variable.
  *
  * The binding lives in a scope of its own per item, so the loop variable does not leak and an assignment inside
  * the expression behaves as it does in a `\for` body.
  */
private def withBoundItems(proc: Processor, pos: CharReader): (Vector[Value], Value => Value) =
  val varName = proc.readControlSeqName(pos)
  proc.skipSpaces()
  val seq  = proc.evalArgumentExpr(pos)
  val body = stripOuterBraces(proc.readArgument(pos))

  def evalFor(item: Value): Value =
    proc.handler.enterScope()
    try
      proc.handler.set(varName, item)
      evalBody(proc, body, pos)
    finally proc.handler.exitScope()

  (itemsOf(seq), evalFor)

/** Evaluate a body for its value, where the body may be more than one statement — the semantics a macro body has
  * in expression position, shared with it so a `\filter` condition and a macro return read the same way. See
  * `Processor.evalBodyExpr`.
  */
private def evalBody(proc: Processor, body: Vector[Token], pos: CharReader): Value =
  proc.evalBodyExpr(body, pos)

/** `\filter \var {seq} {condition}` — the items for which the condition is true, in their original order. */
object FilterPrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    val (items, evalFor) = withBoundItems(proc, pos)

    valueResult(proc, Value.Seq(items.filter(item => Value.truthy(evalFor(item)))))

/** `\transform \var {seq} {expression}` — each item replaced by what the expression computes from it. This is the
  * map operation; it is not called `\map` because that name already builds a map literal.
  */
object TransformPrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    val (items, evalFor) = withBoundItems(proc, pos)

    valueResult(proc, Value.Seq(items.map(evalFor)))

/** `\sortby \var {seq} {key}` — the items ordered by a key computed from each, rather than by the items
  * themselves: a list of records sorted on one field, or a list of words sorted case-insensitively with
  * `\downcase{\w}`. The sort is stable, so items with equal keys keep their original order and the document is
  * byte-identical from one pass to the next.
  */
object SortByPrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    val (items, evalFor) = withBoundItems(proc, pos)
    val keyed            = items.map(item => (evalFor(item), item))

    valueResult(proc, Value.Seq(stableSortBy(keyed).map(_._2)))

/** `\sort {seq}` — the items in order: numeric where they are numbers, alphabetical where they are words. Stable,
  * for the same reason `\sortby` is.
  */
object SortPrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    val items = itemsOf(proc.evalArgumentExpr(pos))

    valueResult(proc, Value.Seq(stableSortBy(items.map(i => (i, i))).map(_._2)))

/** Sort key-value pairs by key, keeping the original order of equal keys. `sortWith` is not guaranteed stable
  * across the platforms texish cross-builds for, and an unstable sort would make a document's output depend on the
  * sort implementation — so the index is carried in the comparison, which fixes the order by construction.
  */
private def stableSortBy(keyed: Vector[(Value, Value)]): Vector[(Value, Value)] =
  keyed.zipWithIndex
    .sortWith { case (((k1, _), i1), ((k2, _), i2)) =>
      val c = compareValues(k1, k2)
      if c != 0 then c < 0 else i1 < i2
    }
    .map(_._1)

// ============ STRINGS ============

/** `\split {text} {separator}` — the text cut at each occurrence of the separator, as a sequence. An empty
  * separator splits into characters. The separator is matched literally, not as a pattern, so a `.` or a `|` in it
  * means itself — which is what a document author writing `\split{\csv}{,}` expects.
  */
object SplitPrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    val text = Value.display(proc.evalStringArgument(pos))
    val sep  = Value.display(proc.evalStringArgument(pos))
    val parts =
      if sep.isEmpty then codePointStrings(text)
      else
        val out   = Vector.newBuilder[String]
        var start = 0
        var at    = text.indexOf(sep)
        while at >= 0 do
          out += text.substring(start, at)
          start = at + sep.length
          at = text.indexOf(sep, start)
        out += text.substring(start)
        out.result()

    valueResult(proc, Value.Seq(parts.map(Value.Text.apply)))

/** `\replace {text} {from} {to}` — every occurrence of `from` replaced by `to`, matched literally. An empty `from`
  * would match everywhere and never advance, so it leaves the text alone.
  */
object ReplacePrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    val text = Value.display(proc.evalStringArgument(pos))
    val from = Value.display(proc.evalStringArgument(pos))
    val to   = Value.display(proc.evalStringArgument(pos))
    val out =
      if from.isEmpty then text
      else
        val sb    = new StringBuilder
        var start = 0
        var at    = text.indexOf(from)
        while at >= 0 do
          sb.append(text.substring(start, at)).append(to)
          start = at + from.length
          at = text.indexOf(from, start)
        sb.append(text.substring(start)).toString

    valueResult(proc, Value.Text(out))

/** `\repeat {text} {n}` — the text `n` times over, for a rule of dots, an indent, or a bar of a chart drawn in
  * characters. A count of zero or less gives the empty string.
  */
object RepeatPrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    val text = Value.display(proc.evalStringArgument(pos))
    val n    = Value.number(proc.evalArgumentExpr(pos)).map(_.toInt).getOrElse(0)

    valueResult(proc, Value.Text(if n <= 0 then "" else text * n))

/** `\startswith {text} {prefix}` / `\endswith {text} {suffix}` — Bools, for dispatching on a marker without
  * slicing the string apart first.
  */
private class AffixPrimitive(test: (String, String) => Boolean) extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    val text  = Value.display(proc.evalStringArgument(pos))
    val affix = Value.display(proc.evalStringArgument(pos))

    valueResult(proc, Value.Bool(test(text, affix)))

object StartsWithPrimitive extends AffixPrimitive(_.startsWith(_))
object EndsWithPrimitive extends AffixPrimitive(_.endsWith(_))

/** `\fixed {number} {places}` — a number as text with exactly that many decimal places, zeros included: `\fixed`
  * of 0.3 to 2 places is "0.30" where `\round` gives "0.3". A column of prices or measurements wants the trailing
  * zeros; `\round` is for cleaning floating-point noise out of a computed value, and the two are not the same job.
  */
object FixedPrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    val v      = proc.evalArgumentExpr(pos)
    val x      = Value.number(v).getOrElse(proc.handler.error(s"\\fixed: '${Value.display(v)}' is not a number", pos))
    val places = Value.number(proc.evalArgumentExpr(pos)).map(_.toInt).getOrElse(0)

    if places < 0 || places > 15 then proc.handler.error(s"\\fixed: places must be between 0 and 15, got $places", pos)

    // built from the rounded integer rather than through a format string, which Scala Native does not implement
    // in full — and this way the padding is explicit rather than locale-dependent
    val out =
      if places == 0 then math.round(x).toString
      else
        val scale = math.round(math.pow(10, places))
        val total = math.round(math.abs(x) * scale)
        val whole = total / scale
        val frac  = (total % scale).toString
        val sign  = if x < 0 && total != 0 then "-" else ""

        s"$sign$whole.${"0" * (places - frac.length)}$frac"

    valueResult(proc, Value.Text(out))

// ============ MAPS ============

/** `\keys {map}` / `\values {map}` — a map's keys or its values, as a sequence, in the order the map was built.
  * With these a map can be walked by a `\for` over its keys, sorted, filtered or counted like any other sequence.
  */
object KeysPrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    val result = proc.evalArgumentExpr(pos) match
      case Value.Map(entries) => Value.Seq(entries.keys.toVector.map(Value.Text.apply))
      case _                  => Value.Seq(Vector.empty)

    valueResult(proc, result)

object ValuesPrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    val result = proc.evalArgumentExpr(pos) match
      case Value.Map(entries) => Value.Seq(entries.values.toVector)
      case _                  => Value.Seq(Vector.empty)

    valueResult(proc, result)

/** `\mapdel name {key}` — remove a key from the map variable `name`, the counterpart of `\mapset`. Honours a
  * `\global` prefix, like `\mapset`, so a global store can be cleared from inside a group. Removing a key that is
  * not there leaves the map alone.
  */
object MapDelPrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    val name   = proc.readIdentifier(pos)
    val global = proc.handler.globalAssign
    proc.handler.globalAssign = false
    val key = Value.display(proc.evalArgumentExpr(pos))

    proc.handler.get(name) match
      case Value.Map(m) =>
        val updated = Value.Map(m - key)
        if global then proc.handler.setGlobal(name, updated) else proc.handler.set(name, updated)
      case _ => ()
