package io.github.edadma.texish

/** The mapping from input to math atoms: from the single characters typed inside `$…$` and from
  * control-sequence names (`\alpha`, `\leq`, `\sum`, …) to a Unicode codepoint in the math font plus the
  * [[MathClass]] that determines the spacing around it.
  *
  * Following the unicode-math model a real math font implements, letters become *mathematical italic*
  * variables — their own Unicode block, which a font like Latin Modern Math provides — while digits, large
  * operators and most symbols are upright. The class is what matters for spacing: `+` is `Bin`, `=` is
  * `Rel`, `(` is `Open`, and so on. This object is pure data and the classification logic over it; it holds
  * no font and no engine state, so it is trivially testable.
  */
object MathSymbols:
  import MathClass.*

  /** An ASCII letter's Mathematical Italic codepoint. The block is contiguous except for lowercase `h`,
    * whose italic form lives at U+210E (ℎ, the slot Unicode reused for the Planck constant). */
  private def italicLetter(c: Char): Int =
    if c == 'h' then 0x210E
    else if c >= 'a' && c <= 'z' then 0x1D44E + (c - 'a')
    else 0x1D434 + (c - 'A')

  /** Script (calligraphic) letters live in their own contiguous Mathematical Alphanumeric blocks, except for
    * the dozen that Unicode had already encoded in the Letterlike Symbols block and unified there rather than
    * duplicate. This table carries those exceptions; everything else is offset arithmetic. */
  private val scriptExceptions: Map[Char, Int] = Map(
    'B' -> 0x212C, 'E' -> 0x2130, 'F' -> 0x2131, 'H' -> 0x210B, 'I' -> 0x2110,
    'L' -> 0x2112, 'M' -> 0x2133, 'R' -> 0x211B,
    'e' -> 0x212F, 'g' -> 0x210A, 'o' -> 0x2134,
  )

  /** The Mathematical Script codepoint for an ASCII letter — the alphabet `\mathcal` selects — or None for a
    * character that has no script form (digits, punctuation). */
  def scriptLetter(c: Char): Option[Int] =
    scriptExceptions.get(c).orElse(
      if c >= 'A' && c <= 'Z' then Some(0x1D49C + (c - 'A'))
      else if c >= 'a' && c <= 'z' then Some(0x1D4B6 + (c - 'a'))
      else None,
    )

  /** A calligraphic atom for one `\mathcal` letter, or None for a character with no script form (so the caller
    * can fall back to ordinary classification for a stray digit or symbol in the argument). */
  def mathcalNode(mf: MathFont, codepoint: Int): Option[MathNode] =
    scriptLetter(codepoint.toChar).map(cp => glyphAtom(mf, Ord, cp))

  // Single non-alphanumeric characters: the codepoint to set (often a dedicated math character rather than
  // the ASCII one) and the atom class.
  private val chars: Map[Char, (Int, MathClass)] = Map(
    '+'  -> (0x2B, Bin),
    '-'  -> (0x2212, Bin),  // MINUS SIGN, not the hyphen-minus that was typed
    '*'  -> (0x2217, Bin),  // ASTERISK OPERATOR
    '/'  -> (0x2F, Ord),
    '='  -> (0x3D, Rel),
    '<'  -> (0x3C, Rel),
    '>'  -> (0x3E, Rel),
    '('  -> (0x28, Open),
    '['  -> (0x5B, Open),
    ')'  -> (0x29, Close),
    ']'  -> (0x5D, Close),
    '|'  -> (0x7C, Ord),
    '.'  -> (0x2E, Ord),
    ','  -> (0x2C, Punct),
    ';'  -> (0x3B, Punct),
    ':'  -> (0x3A, Rel),
    '!'  -> (0x21, Close),
    '?'  -> (0x3F, Close),
    '\'' -> (0x2032, Ord), // PRIME
  )

  // Control-sequence symbols: name (without the backslash) to codepoint and class.
  private val symbols: Map[String, (Int, MathClass)] = Map(
    // Lowercase Greek — mathematical italic variables.
    "alpha"   -> (0x1D6FC, Ord), "beta"  -> (0x1D6FD, Ord), "gamma"   -> (0x1D6FE, Ord),
    "delta"   -> (0x1D6FF, Ord), "epsilon" -> (0x1D700, Ord), "zeta"  -> (0x1D701, Ord),
    "eta"     -> (0x1D702, Ord), "theta" -> (0x1D703, Ord), "iota"    -> (0x1D704, Ord),
    "kappa"   -> (0x1D705, Ord), "lambda" -> (0x1D706, Ord), "mu"     -> (0x1D707, Ord),
    "nu"      -> (0x1D708, Ord), "xi"    -> (0x1D709, Ord), "pi"      -> (0x1D70B, Ord),
    "rho"     -> (0x1D70C, Ord), "sigma" -> (0x1D70E, Ord), "tau"     -> (0x1D70F, Ord),
    "upsilon" -> (0x1D710, Ord), "phi"   -> (0x1D711, Ord), "chi"     -> (0x1D712, Ord),
    "psi"     -> (0x1D713, Ord), "omega" -> (0x1D714, Ord),
    "varepsilon" -> (0x1D716, Ord), "vartheta" -> (0x1D717, Ord), "varphi" -> (0x1D719, Ord),

    // Uppercase Greek — upright, as in Computer Modern and Latin Modern math.
    "Gamma" -> (0x0393, Ord), "Delta" -> (0x0394, Ord), "Theta" -> (0x0398, Ord),
    "Lambda" -> (0x039B, Ord), "Xi" -> (0x039E, Ord), "Pi" -> (0x03A0, Ord),
    "Sigma" -> (0x03A3, Ord), "Upsilon" -> (0x03A5, Ord), "Phi" -> (0x03A6, Ord),
    "Psi" -> (0x03A8, Ord), "Omega" -> (0x03A9, Ord),

    // Binary operators.
    "times" -> (0x00D7, Bin), "div" -> (0x00F7, Bin), "pm" -> (0x00B1, Bin),
    "mp" -> (0x2213, Bin), "cdot" -> (0x22C5, Bin), "ast" -> (0x2217, Bin),
    "star" -> (0x22C6, Bin), "circ" -> (0x2218, Bin), "bullet" -> (0x2219, Bin),
    "oplus" -> (0x2295, Bin), "ominus" -> (0x2296, Bin), "otimes" -> (0x2297, Bin),
    "oslash" -> (0x2298, Bin), "odot" -> (0x2299, Bin), "wedge" -> (0x2227, Bin),
    "land" -> (0x2227, Bin), "vee" -> (0x2228, Bin), "lor" -> (0x2228, Bin),
    "cap" -> (0x2229, Bin), "cup" -> (0x222A, Bin), "setminus" -> (0x2216, Bin),
    "dagger" -> (0x2020, Bin), "ddagger" -> (0x2021, Bin), "amalg" -> (0x2A3F, Bin),

    // Relations.
    "leq" -> (0x2264, Rel), "le" -> (0x2264, Rel), "geq" -> (0x2265, Rel), "ge" -> (0x2265, Rel),
    "neq" -> (0x2260, Rel), "ne" -> (0x2260, Rel), "equiv" -> (0x2261, Rel),
    "approx" -> (0x2248, Rel), "sim" -> (0x223C, Rel), "simeq" -> (0x2243, Rel),
    "cong" -> (0x2245, Rel), "propto" -> (0x221D, Rel), "asymp" -> (0x224D, Rel),
    "doteq" -> (0x2250, Rel), "ll" -> (0x226A, Rel), "gg" -> (0x226B, Rel),
    "prec" -> (0x227A, Rel), "succ" -> (0x227B, Rel),
    "in" -> (0x2208, Rel), "notin" -> (0x2209, Rel), "ni" -> (0x220B, Rel),
    "subset" -> (0x2282, Rel), "supset" -> (0x2283, Rel),
    "subseteq" -> (0x2286, Rel), "supseteq" -> (0x2287, Rel),
    "perp" -> (0x22A5, Rel), "models" -> (0x22A8, Rel), "mid" -> (0x2223, Rel),
    "parallel" -> (0x2225, Rel),

    // Arrows — relations.
    "leftarrow" -> (0x2190, Rel), "gets" -> (0x2190, Rel), "rightarrow" -> (0x2192, Rel),
    "to" -> (0x2192, Rel), "leftrightarrow" -> (0x2194, Rel), "mapsto" -> (0x21A6, Rel),
    "Leftarrow" -> (0x21D0, Rel), "Rightarrow" -> (0x21D2, Rel), "Leftrightarrow" -> (0x21D4, Rel),
    "uparrow" -> (0x2191, Rel), "downarrow" -> (0x2193, Rel),
    "longleftarrow" -> (0x27F5, Rel), "longrightarrow" -> (0x27F6, Rel),
    "longleftrightarrow" -> (0x27F7, Rel), "longmapsto" -> (0x27FC, Rel),
    "rightharpoonup" -> (0x21C0, Rel), "rightleftharpoons" -> (0x21CC, Rel),

    // Ordinary symbols.
    "infty" -> (0x221E, Ord), "partial" -> (0x2202, Ord), "nabla" -> (0x2207, Ord),
    "forall" -> (0x2200, Ord), "exists" -> (0x2203, Ord), "emptyset" -> (0x2205, Ord),
    "neg" -> (0x00AC, Ord), "lnot" -> (0x00AC, Ord), "prime" -> (0x2032, Ord),
    "hbar" -> (0x210F, Ord), "ell" -> (0x2113, Ord), "wp" -> (0x2118, Ord),
    "Re" -> (0x211C, Ord), "Im" -> (0x2111, Ord), "aleph" -> (0x2135, Ord),
    "angle" -> (0x2220, Ord), "triangle" -> (0x25B3, Ord), "surd" -> (0x221A, Ord),
    "top" -> (0x22A4, Ord), "bot" -> (0x22A5, Ord), "flat" -> (0x266D, Ord),
    "natural" -> (0x266E, Ord), "sharp" -> (0x266F, Ord),

    // Large operators.
    "sum" -> (0x2211, Op), "prod" -> (0x220F, Op), "coprod" -> (0x2210, Op),
    "int" -> (0x222B, Op), "oint" -> (0x222E, Op), "iint" -> (0x222C, Op),
    "iiint" -> (0x222D, Op), "bigcup" -> (0x22C3, Op), "bigcap" -> (0x22C2, Op),
    "bigoplus" -> (0x2A01, Op), "bigotimes" -> (0x2A02, Op), "bigvee" -> (0x22C1, Op),
    "bigwedge" -> (0x22C0, Op),

    // Fences as bare symbols (not yet stretchy — that is Stage 5).
    "langle" -> (0x27E8, Open), "rangle" -> (0x27E9, Close),
    "lbrace" -> (0x7B, Open), "rbrace" -> (0x7D, Close),
    "lfloor" -> (0x230A, Open), "rfloor" -> (0x230B, Close),
    "lceil" -> (0x2308, Open), "rceil" -> (0x2309, Close),

    // Inner / dots.
    "ldots" -> (0x2026, Inner), "cdots" -> (0x22EF, Inner),
    "vdots" -> (0x22EE, Inner), "ddots" -> (0x22F1, Inner),
  )

  // Multi-letter operator names set upright (\sin, \log, …). They are Op-class atoms whose nucleus is a
  // little row of upright Latin glyphs.
  private val operatorNames: Set[String] = Set(
    "sin", "cos", "tan", "cot", "sec", "csc",
    "sinh", "cosh", "tanh", "coth",
    "log", "ln", "lg", "exp", "lim", "limsup", "liminf",
    "max", "min", "sup", "inf", "det", "gcd", "deg", "arg", "dim", "ker", "hom",
  )

  // Explicit math spaces, in math units (18mu = 1em). \! is a negative thin space.
  private val spacesMu: Map[String, Double] = Map(
    "," -> 3.0, ":" -> 4.0, ";" -> 5.0, "!" -> -3.0, "quad" -> 18.0, "qquad" -> 36.0,
  )

  /** Classify one input character into a math node, or `None` for a character with no math meaning. Letters
    * become italic variables, digits stay upright, and the punctuation/operator table covers the rest. */
  def charNode(mf: MathFont, codepoint: Int): Option[MathNode] =
    val c = codepoint.toChar

    if c < 128 && c.isLetter then Some(glyphAtom(mf, Ord, italicLetter(c)))
    else if c.isDigit then Some(glyphAtom(mf, Ord, codepoint)) // upright digit
    else chars.get(c).map((cp, cls) => glyphAtom(mf, cls, cp))

  /** Classify a control-sequence name into a math node, or `None` if it is not a known math symbol, an
    * operator name, or an explicit space. */
  def commandNode(mf: MathFont, name: String): Option[MathNode] =
    symbols.get(name).map((cp, cls) => glyphAtom(mf, cls, cp))
      .orElse(if operatorNames(name) then Some(MathAtom(Op, operatorBox(mf, name))) else None)
      .orElse(spacesMu.get(name).map(mu => MathSpace(Glue(mu / 18.0 * mf.size))))

  /** A single-glyph atom carrying that glyph's italic correction, so a superscript can be set out past a
    * slanted nucleus (a variable, the integral sign) by the right amount. A large operator (`\sum`, `\int`)
    * also records its codepoint, so `\limits` can grow it to a display-size variant. */
  private def glyphAtom(mf: MathFont, cls: MathClass, codepoint: Int): MathAtom =
    MathAtom(
      cls,
      mf.glyphBox(codepoint),
      italicCorrection = mf.italicCorrection(codepoint),
      bigOp = if cls == Op then Some(codepoint) else None,
      nucleusCp = Some(codepoint),
    )

  /** An upright row of Latin glyphs for an operator name like `sin`. */
  private def operatorBox(mf: MathFont, name: String): Box =
    HBox(name.map(ch => mf.glyphBox(ch.toInt)).toVector)
