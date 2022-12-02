package org.justinhj

import scala.util.Try
import cats.Monoid
import cats.implicits._

object Adventofcode2021day1monoid extends App {

  import AOCUtil.inputToStrings

  // Couple of helpers to create a PenguinGrouper from an int and from nothing
  object PenguinGrouper {
    def apply(a: Int): PenguinGrouper = PenguinGrouper(a, List.empty[Int], false, false)
    def empty: PenguinGrouper = PenguinGrouper(0, List.empty[Int], true, true)
  }

  // This is the type we will use monoid append to group the penguins
  case class PenguinGrouper(current: Int, groups: List[Int], leftBoundary: Boolean, rightBoundary: Boolean)

  // Monoidal instance for PenguinGrouper
  implicit val penguinGrouperMonoid = new Monoid[PenguinGrouper] {
    def empty = PenguinGrouper(0, List.empty[Int], true, true)
    def combine(x: PenguinGrouper, y: PenguinGrouper): PenguinGrouper = {
      // Two cases to handle. The normal case we just build the current list
      // and propagate the left and right boundaries. The second case is when
      // left and right boundaries just became true, in which case collapse the
      // list into the groups of penguins
      val leftBoundary = x.leftBoundary
      val rightBoundary = y.rightBoundary
      val current = x.current + y.current
      if (leftBoundary == true && leftBoundary == rightBoundary && 
            (x.leftBoundary == false 
            || x.rightBoundary == false 
            || y.leftBoundary == false 
            || y.rightBoundary == false)) {
        if(current.isEmpty) PenguinGrouper(0, x.groups ++ y.groups, leftBoundary, rightBoundary)
        else PenguinGrouper(0, x.groups ++ y.groups :+ current, leftBoundary, rightBoundary)
      } else {
        PenguinGrouper(current, x.groups ++ y.groups, leftBoundary, rightBoundary)
      }
    }
  }

  def parseInput(input: List[String]): List[Option[Int]] = 
    input.map {a => Try(a.toInt).toOption}

  def solve(in: List[Option[Int]]): Int = {
    // Monoidal solve
    // First turn the list of options into PenguinGrouper's
    val pgs = in.map {
        case Some(a) => PenguinGrouper(a)
        case None => PenguinGrouper.empty
      }
    val groups = pgs.combineAll
    groups.groups.max  
  }


  // Part 1 - elf with most calories
  val exampleInput = inputToStrings("example.txt")
  val exampleLines = parseInput(exampleInput)
  println(solve(exampleLines))

  val input1 = inputToStrings("day1.txt")
  val input1Lines = parseInput(input1)
  println(solve(input1Lines))

  // Part 2 - top 3 elves
  // Can just modify the solve function a bit

  def solve2(in: List[Option[Int]]): Int = {
    val pgs = in.appended(None).map {
        case Some(a) => PenguinGrouper(a)
        case None => PenguinGrouper.empty
      }
    val groups = pgs.combineAll
    val penguins = groups.groups
    penguins.sorted.reverse.take(3).sum
  }

  println(solve2(exampleLines))
  println(solve2(input1Lines))
}
