package io.github.edadma.texish

/** Inter-atom spacing in math mode. TeX measures these spaces in *math units* (mu), where 18mu make one
  * em, so they scale with the math font's size. Between two atoms the engine inserts one of three named
  * spaces — thin (3mu), medium (4mu, stretchable) or thick (5mu) — chosen from a table indexed by the
  * class of the atom on the left and the class of the atom on the right. The table mirrors the one in the
  * TeXbook (Chapter 18); entries shown there in parentheses are suppressed in script and scriptscript
  * styles, but Stage 2 sets type in text style only, so they all apply here.
  */
object MathSpacing:
  import MathClass.*

  private val order = Vector(Ord, Op, Bin, Rel, Open, Close, Punct, Inner)
  private val idx   = order.zipWithIndex.toMap

  // Rows indexed by the left atom's class, columns by the right atom's class, in `order`. Values are
  // spacing codes: 0 none, 1 thin, 2 medium, 3 thick. The TeXbook marks some pairs impossible ('*'); they
  // are encoded as 0 here because the bin-normalization pass in [[MathList]] removes every Bin atom that
  // could stand on one side of such a pair, so they never actually arise.
  private val table: Vector[Vector[Int]] = Vector(
    //              Ord Op Bin Rel Opn Cls Pun Inr
    /* Ord   */ Vector(0, 1, 2, 3, 0, 0, 0, 1),
    /* Op    */ Vector(1, 1, 0, 3, 0, 0, 0, 1),
    /* Bin   */ Vector(2, 2, 0, 0, 2, 0, 0, 2),
    /* Rel   */ Vector(3, 3, 0, 0, 3, 0, 0, 3),
    /* Open  */ Vector(0, 0, 0, 0, 0, 0, 0, 0),
    /* Close */ Vector(0, 1, 2, 3, 0, 0, 0, 1),
    /* Punct */ Vector(1, 1, 0, 1, 1, 1, 1, 1),
    /* Inner */ Vector(1, 1, 2, 3, 1, 0, 1, 1),
  )

  /** The spacing code (0 none, 1 thin, 2 medium, 3 thick) between a left atom and a right atom. */
  def code(left: MathClass, right: MathClass): Int = table(idx(left))(idx(right))

  /** The glue for a spacing code at the given em (the math font's size in points), or `None` for code 0.
    * Thin is rigid (\thinmuskip); medium and thick stretch as \medmuskip and \thickmuskip do. */
  def glue(code: Int, em: Double): Option[Glue] =
    def mu(n: Double): Double = n / 18.0 * em

    code match
      case 1 => Some(Glue(mu(3)))                  // thin: 3mu
      case 2 => Some(Glue(mu(4), mu(2), mu(4)))    // medium: 4mu plus 2mu minus 4mu
      case 3 => Some(Glue(mu(5), mu(5), 0))        // thick: 5mu plus 5mu
      case _ => None
