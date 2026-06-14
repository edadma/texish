package io.github.edadma.texish

object Abbreviation:
  val words: Set[String] =
    Set(
      // titles
      "Dr",
      "Esq",
      "Hon",
      "Jr",
      "Mr",
      "Mrs",
      "Ms",
      "Messrs",
      "Mmes",
      "Msgr",
      "Prof",
      "Rev",
      "Hon",
      "Sr",
      "St",

      // Bible
      "Isa",
      "Hab",
      "Ps",
      "Heb",
      "Gen",
      "Pet",
      "Ezek",
      "Matt",
      "Jer",
      "Cor",
      "Prov",
      "Pro",
      "Tim",
      "Thess",
      "Thes",
      "Rev",
      "Sam",
      "Ex",
      "Gal",
      "Rom",
      "Mal",
      "Dan",
      "Zech",
      "Eph",
      "Num",
      "Lev",
      "Eccl",
      "Exo",
      "Hos",
      "Eccl",
      "Deut",
      "Chron",
    )

  def apply(s: String): Boolean = words(s)
