package day02

import zio.*
import zio.test.*
import zio.test.TestAspect.*

import scala.annotation.tailrec
import scala.math.*

// ------------------------------------------------------------------------------
def parse(input: List[String]): List[List[Int]] =
  input
    .map(_.trim.split("\\s+").toList)
    .map(_.map(_.toInt))

def isInc(a: Int, b: Int): Boolean = a < b && (b - a <= 3)
def isDec(a: Int, b: Int): Boolean = a > b && (a - b <= 3)

// ------------------------------------------------------------------------------

def reportCheck(report: List[Int], check: (Int, Int) => Boolean): Boolean = {
  report.sliding(2, 1).forall { case List(a, b) => check(a, b) }
}

def resolveStar1(input: List[String]): Int = {
  val reports = parse(input)
  reports.count(report => reportCheck(report, isInc) || reportCheck(report, isDec))
}

// ------------------------------------------------------------------------------

def fixedReports(report: List[Int]): LazyList[List[Int]] = {
  LazyList
    .from(0)
    .take(report.size)
    .map(i => report.patch(i, Nil, 1))
}

def reportFixableCheck(report: List[Int], check: (Int, Int) => Boolean): Boolean = {
  def candidates = report #:: fixedReports(report)
  candidates.exists(candidate => candidate.sliding(2, 1).forall { case List(a, b) => check(a, b) })
}

def resolveStar2(input: List[String]): Int = {
  val reports = parse(input)
  reports.count(report => reportFixableCheck(report, isInc) || reportFixableCheck(report, isDec))
}

// ------------------------------------------------------------------------------

object Puzzle02Test extends ZIOSpecDefault {
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
        exampleResult == 2,
        puzzleResult == 564
      )
    },
    test("star#2") {
      for {
        exampleInput1 <- fileLines(Path(s"data/$day/example-1.txt"))
        exampleResult1 = resolveStar2(exampleInput1)
        puzzleInput   <- fileLines(Path(s"data/$day/puzzle-1.txt"))
        puzzleResult   = resolveStar2(puzzleInput)
      } yield assertTrue(
        exampleResult1 == 4,
        puzzleResult == 604
      )
    }
  ) @@ timed @@ sequential
}
