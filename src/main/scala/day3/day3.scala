package day3

import scala.io.Source

object day3 extends App {
  case class adventure(treesHit: Int, x: Int, y: Int)

  val hitTrees = (pattern: List[String], goRight: Int, goDown: Int) =>
    pattern
      .foldLeft(adventure(0,0,0))((acc, curr) => {
        if (acc.y % goDown == 0) {
          val treeDamage = if (curr(acc.x % pattern(0).length) == '#') 1 else 0
          adventure(acc.treesHit + treeDamage, acc.x + goRight, acc.y + 1)
        }
        else adventure(acc.treesHit, acc.x, acc.y + 1)
      })
      ._1

  val checkTreePaths = (pattern: List[String]) => {
    List(
      hitTrees(pattern, 1, 1),
      hitTrees(pattern, 3, 1),
      hitTrees(pattern, 5, 1),
      hitTrees(pattern, 7, 1),
      hitTrees(pattern, 1, 2)
    ).map(x => BigInt(x)).product
  }

//  println(hitTrees(Source.fromFile("src/main/scala/day3/day3.txt").getLines().toList, 3, 1))
  println(checkTreePaths(Source.fromFile("src/main/scala/day3/day3.txt").getLines().toList))
}
