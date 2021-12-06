package day4

import scala.io.Source

object day4 extends App {
  case class Passport(
    birthYear: String,
    issueYear: String,
    expirationYear: String,
    height: String,
    hairColor: String,
    eyeColor: String,
    passportId: String,
    countryId: String
  ) {
    def isValid(): Boolean =
      List(birthYear, issueYear, expirationYear, height, hairColor, eyeColor, passportId)
        .filterNot(_.isEmpty).size == 7
  }

  object Passport {
    def apply(): Passport =
      new Passport("", "", "", "",
      "", "", "", "")

    def generate(passString: List[String]): Passport = {
      passString.foldLeft(Passport())((acc, curr) => {
        val pass = curr match {
          case Passport.birthYearRegex(n) => acc.copy(birthYear = n)
          case Passport.issueYearRegex(n) => acc.copy(issueYear = n)
          case Passport.expYearRegex(n) => acc.copy(expirationYear = n)
          case Passport.heightRegex(n) => acc.copy(height = n)
          case Passport.hairColorRegex(n) => acc.copy(hairColor = n)
          case Passport.eyeColorRegex(n) => acc.copy(eyeColor = n)
          case Passport.passportIdRegex(n) => acc.copy(passportId = n)
          case Passport.countryIdRegex(n) => acc.copy(countryId = n)
        }
        pass
      })
    }

    def birthYearRegex = "byr:([#a-z0-9]*)".r
    def issueYearRegex = "iyr:([#a-z0-9]*)".r
    def expYearRegex = "eyr:([#a-z0-9]*)".r
    def heightRegex = "hgt:([#a-z0-9]*)".r
    def hairColorRegex = "hcl:([#a-z0-9]*)".r
    def eyeColorRegex = "ecl:([#a-z0-9]*)".r
    def passportIdRegex = "pid:([#a-z0-9]*)".r
    def countryIdRegex = "cid:([#a-z0-9]*)".r
  }

  val readPassports = (input: List[String]) => {
    input
      .foldLeft(List[List[String]](List()))((acc, curr) => {
        if (curr.isEmpty) {
          acc :+ List[String]()
        }
        else {
          acc.updated(acc.size - 1, acc.last :+ curr)
        }
      })
      .map(x => x.mkString(" "))
      .map(x => x.split(" "))
      .map(x => Passport.generate(x.toList))
      .filter(x => x.isValid())
      .size
  }

  println(readPassports(Source.fromFile("src/main/scala/day4/day4.txt").getLines.toList))
}
