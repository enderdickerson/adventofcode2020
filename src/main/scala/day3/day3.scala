package day3

import scala.io.Source

object day3 extends App {
  val hitTrees = (pattern: List[String]) =>
    pattern
      .foldLeft((0,0))((acc, curr) => {
        (acc._1 + (if (curr(acc._2 % pattern(0).length) == '#') 1 else 0), acc._2 + 3)
      })
      ._1

  println(hitTrees(Source.fromFile("src/main/scala/day3/day3.txt").getLines().toList))
}
