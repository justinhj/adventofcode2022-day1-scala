package org.justinhj

import scala.io.Source
import scala.util.Try

object AOCUtil {
  def inputToStrings(name: String): List[String] = {
    Source.fromResource(name).getLines().toList
  }
}

object Adventofcode2021day1 extends App {

  import AOCUtil.inputToStrings

  def parseInput(input: List[String]): List[Option[Int]] = 
    input.map {a => Try(a.toInt).toOption}

  def splitBy[A](p: A => Boolean, input: List[A]): List[List[A]] = {
    val out = input.foldLeft((List.empty[List[A]], List.empty[A])) {
      case (((acc, cur), a)) =>
        if(!p(a)) (acc, a +: cur)
        else (cur :: acc, List.empty[A])
    }
    out._1 :+ out._2
  }

  def solve(input: List[Option[Int]]): Int = {
    val groups = splitBy((a: Option[Int]) => a.isEmpty, input)
    val flattened = groups.map(e => e.flatten)
    flattened.map(_.sum).max
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

  def solve2(input: List[Option[Int]]): Int = {
    val groups = splitBy((a: Option[Int]) => a.isEmpty, input)
    val flattened = groups.map(e => e.flatten)
    flattened.map(_.sum).sorted.reverse.take(3).sum
  }

  println(solve2(exampleLines))
  println(solve2(input1Lines))
}
