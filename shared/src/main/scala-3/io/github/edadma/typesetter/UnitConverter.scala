package io.github.edadma.typesetter

/** Converts unit-suffixed dimension values to big points (1/72 inch) — the engine's one canonical unit. The typesetter
  * is needed only for font-relative units (em, ex).
  */
class UnitConverter(t: Typesetter):
  def cmToPoints(cm: Double): Double = (cm / 2.54) * 72.0

  def inchesToPoints(inches: Double): Double = inches * 72.0

  def mmToPoints(mm: Double): Double = (mm / 25.4) * 72.0

  def picasToPoints(picas: Double): Double = picas * 12.0

  def toPoints(value: Double, unit: String): Double = unit match
    case "pt" => value // Points
    case "pc" => picasToPoints(value) // Picas (1 pica = 12 points)
    case "in" => inchesToPoints(value) // Inches (1 inch = 72 points)
    case "cm" => cmToPoints(value) // Centimeters (1 cm = 28.3465 points)
    case "mm" => mmToPoints(value) // Millimeters (1 mm = 2.83465 points)
    case "em" => value * t.currentFont.size // Em units (based on current font size)
    case "ex" => value * t.currentFont.xHeight // Ex units (based on current font x-height)
    case _    => throw new IllegalArgumentException(s"Unknown unit: $unit")

  def toPicas(points: Double): (Double, Double, String) =
    val picas           = points / 12
    val remainingPoints = points % 12

    (picas, points, f"${picas}p${remainingPoints}")
