package io.github.edadma.typesetter

/** The standard prelude: document-language macro definitions that layer over the built-in
  * primitives the way a TeX *format* (plain.tex, LaTeX) layers macros over the engine. A consumer
  * processes this once, after registering the typesetting primitives and before the document's own
  * content, so these definitions are available to every document.
  *
  * New standard macros belong here, written in the document language itself, rather than in the
  * primitive registration — a macro is document-language, not an engine primitive. The building
  * blocks they use (`\kern`, `\lower`, `\hbox`, em/ex units, …) are the real primitives.
  */
val standardPrelude: String =
  raw"""
% The traditional TeX logo: a T, the E lowered half an x-height, and an X, kerned together.
\def\TeX{T\kern-.1667em\lower.5ex\hbox{E}\kern-.125em X}
"""
