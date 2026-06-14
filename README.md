# texish

A TeX-style document layout and rendering engine for Scala, cross-built for the JVM, Scala Native,
and Scala.js. Body text is set in Latin Modern Roman, with TeX-style math mode set in the matching
Latin Modern Math through an OpenType `MATH` table. Math covers inline `$…$` and centered display
`$$…$$` (with `\eqno` equation numbers): atoms and spacing, super/subscripts, fractions, radicals
(including higher roots), stretchy delimiters, accents, and big operators whose limits stack in
display style.

It breaks paragraphs into lines and lines into pages the way TeX does — Knuth-Plass line breaking,
Liang hyphenation, legal page breaks with widow/orphan control, footnotes, and glue/kern spacing in
a point-space coordinate system. Documents are written in a small TeX-like language (a `parser`
layer over the engine's primitives, with macros, a standard prelude/"format", `\hbox`/`\vbox`,
`\kern`, `\lower`/`\raise`, the `\TeX` and `\TeXish` logos, units like `pt`/`in`/`em`/`ex`, and
more). Pages render through pluggable backends — a Graphics2D/PDF backend on the JVM and a Cairo
image/PDF backend on Native.

Full documentation is coming — a dedicated docs site will be linked here once it's published.

## License

ISC. See [LICENSE](LICENSE).
