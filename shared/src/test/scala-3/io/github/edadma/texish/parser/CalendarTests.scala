package io.github.edadma.texish.parser

import scala.collection.mutable.ArrayBuffer

import io.github.edadma.texish.{Anchor, Box, HeadlessTypesetter, PictureBox, PictureOp, Typesetter}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** The `calendar` package (packages/calendar.texish) drawn through the full parser onto the picture layer. A month
  * lowers to a title bar, a weekday header, an optional today highlight, the grid lines, and one day number per day
  * of the month — each day number a box placed with a north-east anchor in its cell. These check the parts that
  * carry the calendar's meaning: the day count (so the days-in-month table and the leap-year rule are right), the
  * column the 1st lands in (so the weekday congruence is right), the seven-column width, and the today highlight.
  */
class CalendarTests extends AnyFreeSpec with Matchers:

  private val CellWidth = 34.0 // calcw
  private val GridWidth = 238.0 // 7 * calcw

  private class Capture extends HeadlessTypesetter:
    val pictures = ArrayBuffer[PictureBox]()
    override infix def add(box: Box): Typesetter =
      box match
        case pb: PictureBox => pictures += pb
        case _              =>
      super.add(box)

  private def picture(body: String): PictureBox =
    val t       = new Capture
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)
    Console.withOut(new java.io.ByteArrayOutputStream)(proc.process(s"\\use{calendar}$body"))
    t.pictures should have size 1
    t.pictures.head

  /** The day numbers are exactly the boxes placed with a north-east anchor (the header and title use centre). */
  private def dayPlacements(pb: PictureBox): Vector[PictureOp.Place] =
    pb.displayList.collect { case p @ PictureOp.Place(_, Anchor.NorthEast, _, _) => p }

  /** Fills with no stroke — the title bar, the weekday header band, and the today highlight when present. */
  private def filledRegions(pb: PictureBox): Int =
    pb.displayList.count {
      case PictureOp.Paint(Some(_), None) => true
      case _                              => false
    }

  "a month draws one day number per day of the month" in {
    dayPlacements(picture("\\calendar{2026}{7}")) should have size 31 // July, 31 days
    dayPlacements(picture("\\calendar{2026}{6}")) should have size 30 // June, 30 days
  }

  "February follows the Gregorian leap-year rule" in {
    dayPlacements(picture("\\calendar{2024}{2}")) should have size 29 // 2024 is a leap year
    dayPlacements(picture("\\calendar{2023}{2}")) should have size 28 // 2023 is common
    dayPlacements(picture("\\calendar{2000}{2}")) should have size 29 // divisible by 400
    dayPlacements(picture("\\calendar{1900}{2}")) should have size 28 // a century that is not
  }

  "the 1st lands in the column of its weekday" in {
    // 1 June 2026 is a Monday. With weeks starting on Sunday it is the second column (index 1); the day number is
    // anchored at the cell's top-right, calcw from the left edge of that column less the 4pt inset.
    val sundayStart = dayPlacements(picture("\\calendar{2026}{6}")).head
    sundayStart.x shouldBe (2 * CellWidth - 4) +- 0.001
    // With weeks starting on Monday the same Monday falls in the first column (index 0).
    val mondayStart = dayPlacements(picture("\\set calweekstart {1}\\calendar{2026}{6}")).head
    mondayStart.x shouldBe (1 * CellWidth - 4) +- 0.001
  }

  "the calendar is seven columns wide" in {
    picture("\\calendar{2026}{6}").width shouldBe GridWidth +- 0.001
  }

  "the today highlight adds one filled cell" in {
    // Without \caltoday only the title bar and header band are filled; setting it adds the highlight behind one cell.
    filledRegions(picture("\\calendar{2026}{6}")) shouldBe 2
    filledRegions(picture("\\set caltoday {15}\\calendar{2026}{6}")) shouldBe 3
    // A \caltoday outside the month draws no highlight.
    filledRegions(picture("\\set caltoday {40}\\calendar{2026}{6}")) shouldBe 2
  }
