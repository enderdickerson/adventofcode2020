package day3

import scala.io.Source

object day3 extends App {
  val hitTrees = (pattern: List[String]) =>
    pattern
      .map(x => List.fill(6)(x).mkString(""))
      .foldLeft((0,0))((acc, curr) => (acc._1 + (if (curr(acc._2) == '#') 1 else 0), acc._2 + 3))
      ._1

  println(hitTrees(Source.fromFile("src/main/scala/day3/day3test.txt").getLines().toList))
}
