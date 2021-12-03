package day1

import scala.io.Source

object day1 extends App {
  val findPair = (input: Seq[String], total: Int) => {
    val asInts = input.map(_.toInt)
    val found = asInts.foldLeft(List())((acc: List[Int], curr) => {
      if (!acc.isEmpty) acc
      else {
        val diff = total - curr;
        if (asInts.contains(diff)) diff :: curr :: Nil else acc
      }
    })

    if (found.isEmpty) 0 else found.product
  }

  val findTriple = (input: Seq[String]) => {
    val asInts = input.map(_.toInt)
    asInts.foldLeft(List())((acc: List[Int], curr) => {
      if (!acc.isEmpty) acc
      else {
        val diff = 2020 - curr;
        val pair = findPair(asInts.map(_.toString), diff)
        if (pair != 0) {
          pair :: curr :: Nil
        }
        else acc
      }
    }).product
  }

//  println(findPair(Source.fromFile("src/main/scala/day1/day1.txt").getLines.toSeq, 2020))
  println(findTriple(Source.fromFile("src/main/scala/day1/day1.txt").getLines.toSeq))
}
