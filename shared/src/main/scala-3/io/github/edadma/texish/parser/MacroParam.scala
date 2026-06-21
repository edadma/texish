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
  *   - [[ParamKind.Star]] — a leading `*` flag, declared with a bare `*` in the parameter list. The parameter (named
  *     `star`) expands to `1` when a `*` follows the macro at the call site and `0` when it does not, so a body can
  *     branch with `\if {\star} … \else … \fi`. This is xparse's `s` argument — the `\section*` star form.
  *   - [[ParamKind.Raw]] — a verbatim braced group, declared with the parameter name in angle brackets (`<name>`).
  *     The argument is read as literal characters — no comment, escape, active-char or macro processing — exactly as
  *     `\url`'s argument is, and bound to the parameter as a single text token. This lets a macro take a mini-language
  *     fragment (a grammar, a formula) and parse it itself with the string primitives, so specials like `|`, `::=`,
  *     `//` survive. Like all verbatim reads it works only over live input — a raw argument cannot come through
  *     another macro.
  *
  * The kind is a small sealed set so new shapes (delimited parameters, read up to a literal token) can be added
  * without disturbing existing call sites.
  */
enum ParamKind:
  case Mandatory
  case Optional(default: Vector[Token])
  case Star
  case Raw
