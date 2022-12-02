package org.justinhj

import zio.ZQueue

import scala.io.Source
import scala.util.Try

object AOCUtil {
  def inputToStrings(name: String): List[String] = {
    Source.fromResource(name).getLines().toList
  }
}

object Adventofcode2021day1 extends App {

  import AOCUtil.inputToStrings

  def parseInput(input: List[String]): List[Line] = {
    input.map {
      line =>
        val pattern = """(\d+),(\d+) -> (\d+).(\d+)""".r

        val parsed = Try {
          val pattern(x1,y1,x2,y2) = line
          Line(Vec2(x1.toInt,y1.toInt),Vec2(x2.toInt,y2.toInt))
        }
        parsed.get
    }
  }

  // def solve(lines: List[Line], allowDiagonal: Boolean): Int = {
  //   val allPoints = lines
  //                     .flatMap(p => createLine(p,allowDiagonal))
  //                     .groupMapReduce(identity)(_ => 1)(_ + _)

  //   allPoints.values.count(_ > 1)
  // }

  val exampleInput = inputToStrings("example.txt")
  val exampleLines = parseInput(exampleInput)
  // val exampleSolution = solve(exampleLines,allowDiagonal = false)
  // println(exampleSolution)
  // val exampleSolutionPart2 = solve(exampleLines,allowDiagonal = true)
  // println(exampleSolutionPart2)
}
