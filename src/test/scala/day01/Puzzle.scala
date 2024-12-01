package day01

import zio.*
import zio.test.*
import zio.test.TestAspect.*
import scala.math.*

// ------------------------------------------------------------------------------
def parse(input: List[String]):List[List[Int]] =
  input
    .map(_.trim.split("\\s+", 2).toList)
    .map(_.map(_.toInt))

// ------------------------------------------------------------------------------
def resolveStar1(input: List[String]): Int = {
  val data = parse(input)
  val left = data.map(_.head).sorted
  val right = data.map(_.tail.head).sorted
  val diff = left.zip(right).map{case (a,b) => abs(a-b)}
  diff.sum
}

// ------------------------------------------------------------------------------
def resolveStar2(input: List[String]): Int = {
  val data = parse(input)
  val left = data.map(_.head)
  val right = data.map(_.tail.head)
  left.map(n => n*right.count(n == _)).sum
}

// ------------------------------------------------------------------------------

object Puzzle01Test extends ZIOSpecDefault {
  import zio.nio.file.Path
  import helpers.Helpers.*
  val day  = getClass.getName.replaceAll(""".*Puzzle(\d+)Test.*""", "day$1")
  def spec = suite(s"puzzle $day")(
    test("star#1") {
      for {
        exampleInput <- fileLines(Path(s"data/$day/example-1.txt"))
        exampleResult = resolveStar1(exampleInput)
        puzzleInput  <- fileLines(Path(s"data/$day/puzzle-1.txt"))
        puzzleResult  = resolveStar1(puzzleInput)
      } yield assertTrue(
        exampleResult == 11,
        puzzleResult == 2344935
      )
    },
    test("star#2") {
      for {
        exampleInput1 <- fileLines(Path(s"data/$day/example-1.txt"))
        exampleResult1 = resolveStar2(exampleInput1)
        puzzleInput   <- fileLines(Path(s"data/$day/puzzle-1.txt"))
        puzzleResult   = resolveStar2(puzzleInput)
      } yield assertTrue(
        exampleResult1 == 31,
        puzzleResult == 27647262
      )
    }
  ) @@ timed @@ sequential
}
