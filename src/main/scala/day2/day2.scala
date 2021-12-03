package day2

import scala.io.Source

object day2 extends App {
  val asCommand = "([0-9]*)-([0-9]*) ([A-Za-z]): ([A-Za-z]*)".r

  val checkPass = (min: String, max: String, character: String, password: String) => {
    val count = password.toSeq.filter(x => x.toString == character).size
    if (count <= max.toInt && count >= min.toInt) 1 else 0
  }

  val checkPassComplex = (pos1: String, pos2: String, character: String, password: String) => {
    val matchAtFirst = password.toSeq(pos1.toInt - 1).toString == character
    val matchAtSecond = password.toSeq(pos2.toInt - 1).toString == character
    if (!(matchAtFirst && matchAtSecond) && (matchAtFirst || matchAtSecond)) 1 else 0
  }

  val checkPasswords = (input: Seq[String], f: (min: String, max: String, character: String, password: String) => Int) => {
    input.foldLeft(0)((acc, curr) => {
      curr match {
        case asCommand(min, max, ch, pass) => acc + f(min, max, ch, pass)
      }
    })
  }

//  println(checkPasswords(Source.fromFile("src/main/scala/day2/day2test.txt").getLines.toSeq))
//  println(checkPasswords(Source.fromFile("src/main/scala/day2/day2.txt").getLines.toSeq, checkPass))
  println(checkPasswords(Source.fromFile("src/main/scala/day2/day2.txt").getLines.toSeq, checkPassComplex))
}
