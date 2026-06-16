package io.github.edadma.texish.parser

/** One parameter of a `\def` macro or a `\newenvironment` environment, in texish's named-parameter style: the
  * parameter has a name (used as `\name` inside the body) and a [[ParamKind]] saying how its argument is supplied at
  * the call site. This is the xparse-style argument model — each parameter declares its own shape — rather than
  * TeX's positional `#1…#9`.
  */
case class MacroParam(name: String, kind: ParamKind)

/** How a [[MacroParam]]'s argument is read from the call site.
  *
  *   - [[ParamKind.Mandatory]] — a braced group or a single following token, exactly as a plain `\def` parameter has
  *     always been read.
  *   - [[ParamKind.Optional]] — a bracketed `[…]` argument that may be omitted; when omitted the parameter expands to
  *     `default`. This is LaTeX's optional argument, declared `[name:default]` (or `[name]` for an empty default).
  *
  * The kind is a small sealed set so new shapes (delimited parameters, read up to a literal token) can be added
  * without disturbing existing call sites.
  */
enum ParamKind:
  case Mandatory
  case Optional(default: Vector[Token])
