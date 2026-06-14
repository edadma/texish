package io.github.edadma.texish

/** The delimiters `\left` and `\right` may stretch: the mapping from the character or control-sequence name
  * that names a fence to the Unicode codepoint to set it with. A `.` (and the matching `None` result) is the
  * null delimiter — `\left.` opens a stretchy group with nothing drawn on that side, as in TeX. The angle
  * brackets `<` and `>` map to the proper mathematical angle brackets, following plain TeX's delimiter codes.
  */
object MathDelimiters:

  /** The delimiter a single character names, or `None` for `.` (the null delimiter) and for a character that
    * is not a delimiter. */
  def forChar(c: Char): Option[Int] = c match
    case '.' => None
    case '(' => Some(0x28)
    case ')' => Some(0x29)
    case '[' => Some(0x5B)
    case ']' => Some(0x5D)
    case '|' => Some(0x7C)
    case '/' => Some(0x2F)
    case '<' => Some(0x27E8) // ⟨
    case '>' => Some(0x27E9) // ⟩
    case _   => None

  /** The delimiter a control-sequence name (without the backslash) names, or `None` if it is not a known
    * delimiter. The symbolic names `{`, `}`, `|` come from `\{`, `\}`, `\|`. */
  def forCommand(name: String): Option[Int] = name match
    case "langle"                 => Some(0x27E8)
    case "rangle"                 => Some(0x27E9)
    case "lfloor"                 => Some(0x230A)
    case "rfloor"                 => Some(0x230B)
    case "lceil"                  => Some(0x2308)
    case "rceil"                  => Some(0x2309)
    case "lbrace" | "{"           => Some(0x7B)
    case "rbrace" | "}"           => Some(0x7D)
    case "vert"                   => Some(0x7C)
    case "Vert" | "|"             => Some(0x2016) // ‖ — \| and \Vert
    case "backslash"              => Some(0x5C)
    case "uparrow"                => Some(0x2191)
    case "downarrow"              => Some(0x2193)
    case "updownarrow"            => Some(0x2195)
    case _                        => None
