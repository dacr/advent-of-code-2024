package day03

import zio.*
import zio.test.*
import zio.test.TestAspect.*

import scala.annotation.tailrec
import scala.math.*

// ------------------------------------------------------------------------------

def resolveStar1(input: String): Int = {
  val mulRE = """mul\((\d+),(\d+)\)""".r
  mulRE.findAllIn(input).map { case mulRE(a, b) => a.toInt * b.toInt }.sum
}

// ------------------------------------------------------------------------------

def resolveStar2(input: String): Int = {
  val mulRE   = """mul\((\d+),(\d+)\)""".r
  val instrRE = """(?:(mul)\(\d+,\d+\))|(?:(do)\(\))|(?:(don't)\(\))""".r
  val instrs  = instrRE.findAllIn(input).toList

  @tailrec
  def compute(remain: List[String], current: Int, doit: Boolean): Int = {
    remain match {
      case mulRE(a, b) :: tail if doit            => compute(tail, current + a.toInt * b.toInt, doit)
      case head :: tail if head.contains("don't") => compute(tail, current, false)
      case head :: tail if head.contains("do")    => compute(tail, current, true)
      case head :: tail                           => compute(tail, current, doit)
      case Nil                                    => current
    }
  }
  compute(instrs, 0, true)
}

// ------------------------------------------------------------------------------

object Puzzle03Test extends ZIOSpecDefault {
  import zio.nio.file.Path
  import helpers.Helpers.*
  val day  = getClass.getName.replaceAll(""".*Puzzle(\d+)Test.*""", "day$1")
  def spec = suite(s"puzzle $day")(
    test("star#1") {
      for {
        exampleInput <- fileContent(Path(s"data/$day/example-1.txt"))
        exampleResult = resolveStar1(exampleInput)
        puzzleInput  <- fileContent(Path(s"data/$day/puzzle-1.txt"))
        puzzleResult  = resolveStar1(puzzleInput)
      } yield assertTrue(
        exampleResult == 161,
        puzzleResult == 188741603
      )
    },
    test("star#2") {
      for {
        exampleInput1 <- fileContent(Path(s"data/$day/example-2.txt"))
        exampleResult1 = resolveStar2(exampleInput1)
        puzzleInput   <- fileContent(Path(s"data/$day/puzzle-1.txt"))
        puzzleResult   = resolveStar2(puzzleInput)
      } yield assertTrue(
        exampleResult1 == 48,
        puzzleResult == 67269798
      )
    }
  ) @@ timed @@ sequential
}
