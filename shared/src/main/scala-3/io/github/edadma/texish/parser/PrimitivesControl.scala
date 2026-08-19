package io.github.edadma.texish.parser

import io.github.edadma.char_reader.CharReader
import io.github.edadma.path.Path
import io.github.edadma.texish.{EmbeddedPackages, TexishException, Typesetter}

// ============ FOR LOOP ============

object ForPrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    // Syntax: \for\var{sequence-expr}{body}
    val varName = proc.readControlSeqName(pos)
    proc.skipSpaces()

    // Evaluate sequence expression
    val seqValue = proc.evalArgumentExpr(pos)

    // Read body
    val bodyTokens = proc.readArgument(pos)

    // Get items to iterate, through the same itemsOf that \nth indexes and \size counts by, so that the three
    // can never disagree about what a value's items are. A string iterates by code point, so an astral symbol
    // (an emoji, a math letter) is one iteration rather than two broken surrogate halves; a number iterates as
    // the characters it displays as, which is what a run of digits is once it has been stored; and a map
    // iterates in its own entry order, since \map literals and \mapset build insertion-ordered maps.
    val items: Vector[Value] = itemsOf(seqValue)

    // Iterate
    val length = items.size
    items.zipWithIndex.foreach { (item, idx) =>
      proc.handler.enterScope()

      // Set loop variable
      proc.handler.set(varName, item)

      // Set forloop metadata as a map
      val forloop = Value.Map(Map(
        "index" -> Value.Num(idx + 1),
        "indexz" -> Value.Num(idx),
        "first" -> Value.Bool(idx == 0),
        "last" -> Value.Bool(idx == length - 1),
        "length" -> Value.Num(length),
        "rindex" -> Value.Num(length - idx),
        "rindexz" -> Value.Num(length - idx - 1),
        "element" -> item
      ))
      proc.handler.set("forloop", forloop)

      // Process body
      proc.processTokenList(bodyTokens)

      proc.handler.exitScope()
    }

/** `\while {condition} {body}` — run the body over and over while the condition holds, re-evaluating it each time.
  *
  * `\for` walks a sequence that is known before the loop starts; this is the loop for the case where it is not —
  * consuming input until it runs out, or iterating a computation until it converges. The condition is re-read from
  * its own tokens every time round, so it sees whatever the body changed; as in a `\for` body, an assignment meant
  * to outlive the iteration needs `\global`.
  *
  * A runaway loop is stopped rather than left to hang the run: a document whose condition never goes false is a bug
  * in the document, and the error names the limit and the condition so it can be found. The cap is high enough that
  * no honest loop reaches it.
  */
object WhilePrimitive extends Primitive:
  private val limit = 1000000

  def execute(proc: Processor, pos: CharReader): Unit =
    val cond = stripOuterBraces(proc.readArgument(pos))
    val body = proc.readArgument(pos)
    var runs = 0

    while Value.truthy(proc.evalTokensExpr(cond, pos)) do
      if runs >= limit then
        proc.handler.error(s"\\while: stopped after $limit iterations — the condition never became false", pos)
      runs += 1
      proc.handler.enterScope()
      try proc.processTokenList(body)
      finally proc.handler.exitScope()

object DonePrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit = ()
    // End of for loop - marker only

// ============ FILE INCLUSION ============

object IncludePrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    // Read the file path argument
    val pathTokens = proc.readArgument(pos)
    val path = pathTokens.map {
      case Token.Text(s, _)  => s
      case Token.Space(s, _) => s
      case _                 => ""
    }.mkString.trim

    if path.isEmpty then
      proc.handler.error("\\include requires a file path", pos)

    // A relative path resolves against the directory of the document being processed (as \use does), so a
    // document includes its neighbours regardless of where the CLI was launched from. If nothing is there,
    // the path is tried as given (absolute paths and CWD-relative invocations keep working).
    val resolved =
      try
        val beside = Path(proc.currentDir) / path
        if beside.exists && beside.isFile then beside.toPlatformString else path
      catch case _: Throwable => path

    // Read the file and push its tokens onto the source stack
    try
      val fileReader = CharReader.fromFile(resolved)
      val tokenizer = Tokenizer(fileReader)
      proc.pushTokenizer(tokenizer)
    catch
      case e: Exception =>
        proc.handler.error(s"Cannot include file '$path': ${e.getMessage}", pos)

/** `\use{name}` — import a texish module: locate `name.texish`, load it once with output suppressed (so the file's
  * prose and blank lines emit nothing — only its definitions take effect), and skip a repeat `\use` of the same
  * file. This is the module loader; `\include` remains the raw input that re-reads and typesets a literal path.
  *
  * Resolution searches, in order: the directory of the file doing the `\use`, the current directory, the `packages`
  * folder under [[io.github.edadma.texish.Typesetter.home]] if a host set one, the same folder under `$TEXISHHOME`
  * if that environment variable is set, and a `./packages/` folder under the current directory. The first existing
  * file wins, so a local module shadows a standard one. The search roots are an ordered list, so more roots can be
  * added later without changing the ones already in effect.
  *
  * The `Typesetter.home` root is what lets an installation find the modules it shipped without an environment
  * variable — the same directory its `fonts/` comes from. Most packages live on disk rather than in the artifact
  * (only `base` and `document` are embedded), so without it a packaged texish resolves its fonts and not its
  * packages.
  */
object UsePrimitive extends Primitive:
  def execute(proc: Processor, pos: CharReader): Unit =
    val nameTokens = proc.readArgument(pos)
    val name = nameTokens.map {
      case Token.Text(s, _)  => s
      case Token.Space(s, _) => s
      case _                 => ""
    }.mkString.trim

    if name.isEmpty then proc.handler.error("\\use requires a module name", pos)

    val fileName = if name.endsWith(".texish") then name else s"$name.texish"

    // The texish home a host set, and the environment variable as a fallback for a tree in neither the usual
    // place nor beside the program. TEXISHHOME is read from the live environment (see PlatformEnv —
    // `System.getenv`/`sys.env` snapshot it on Native, so a host that set it after start would not be seen).
    val home    = Option(Typesetter.home).filter(_.nonEmpty)
    val envHome = PlatformEnv.get("TEXISHHOME").filter(_.nonEmpty)

    // Search the filesystem first, but tolerate a host that has no working filesystem at all — a browser, where
    // the path layer reaches for Node APIs that are absent. If probing the filesystem throws, treat it as "no
    // file found" and fall through to the embedded copy below.
    val onDisk: Option[Path] =
      try
        val roots: List[Path] =
          List(
            Some(Path(proc.currentDir)),
            Some(Path(".")),
            home.map(h => Path(h) / "packages"),
            envHome.map(h => Path(h) / "packages"),
            Some(Path(".") / "packages"),
          ).flatten
        roots.map(_ / fileName).find(p => p.exists && p.isFile)
      catch case _: Throwable => None

    onDisk match
      case Some(file) =>
        val resolved  = file.toAbsolutePath.normalize
        val canonical = resolved.toPlatformString
        if proc.claimModule(canonical) then
          val dir = resolved.parent.map(_.toPlatformString).getOrElse(".")
          try proc.loadModule(file.readText(), dir)
          catch
            case e: TexishException => throw e
            case e: Exception       => proc.handler.error(s"\\use: cannot load module '$name': ${e.getMessage}", pos)
      case None =>
        // No file on disk: fall back to a module embedded in the build. This is how a host with no package
        // directory — chiefly the browser — resolves the standard modules; where the files are present the
        // search above wins first, so a local package still shadows the embedded copy.
        EmbeddedPackages.sources.get(name.stripSuffix(".texish")) match
          case Some(chunks) =>
            if proc.claimModule(s"embedded:${name.stripSuffix(".texish")}") then proc.loadModule(chunks.mkString, ".")
          case None =>
            proc.handler.error(s"\\use: module '$name' not found on the filesystem or among the embedded modules", pos)
