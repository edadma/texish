package io.github.edadma.texish

/** The eight math atom classes of TeX's Appendix G. Every math symbol belongs to one, and the space the
  * engine inserts between two adjacent symbols is a function of their two classes alone (see
  * [[MathSpacing]]): an ordinary variable, a large operator, a binary operator, a relation, an opening or
  * closing fence, a punctuation mark, or an inner sub-formula.
  */
enum MathClass:
  case Ord, Op, Bin, Rel, Open, Close, Punct, Inner
