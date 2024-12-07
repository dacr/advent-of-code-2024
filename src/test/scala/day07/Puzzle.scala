package day07

import zio.*
import zio.test.*
import zio.test.TestAspect.*

import scala.annotation.tailrec
import scala.math.*
import scala.jdk.StreamConverters.*
import scala.collection.parallel.CollectionConverters.*
import scala.collection.parallel.ForkJoinTaskSupport

// ------------------------------------------------------------------------------
case class Equation(result: Long, values: List[Long])

def parse(input: String): List[Equation] = {
  input.trim
    .lines()
    .toScala(List)
    .flatMap { line =>
      line.split(": ", 2) match {
        case Array(rawResult, rawValues) =>
          val values = rawValues.trim.split(" ").map(_.toLong).toList
          val result = rawResult.trim.toLong
          Some(Equation(result, values))
        case _                           => None
      }
    }
}

// ------------------------------------------------------------------------------

def hasSolutionWithTwoOperatorsWorker(result: Long, computed: Long, remainingValues: List[Long]): Boolean = {
  if (computed > result) false
  else if (remainingValues.isEmpty) computed == result
  else {
    hasSolutionWithTwoOperatorsWorker(result, computed + remainingValues.head, remainingValues.tail) ||
    hasSolutionWithTwoOperatorsWorker(result, computed * remainingValues.head, remainingValues.tail)
  }
}

def hasSolutionWithTwoOperators(equation: Equation): Boolean = {
  equation.values match {
    case first :: tail => hasSolutionWithTwoOperatorsWorker(equation.result, first, tail)
    case _             => false
  }
}

def resolveStar1(input: String): Long = {
  val equations = parse(input)
  equations
    .filter(hasSolutionWithTwoOperators)
    .map(_.result)
    .sum
}

// ------------------------------------------------------------------------------

def concat(a: Long, b: Long): Long = {
  (a.toString + b.toString).toLong
}

def hasSolutionWithThreeOperatorsWorker(result: Long, computed: Long, remainingValues: List[Long]): Boolean = {
  if (computed > result) false
  else if (remainingValues.isEmpty) computed == result
  else {
    hasSolutionWithThreeOperatorsWorker(result, computed + remainingValues.head, remainingValues.tail) ||
    hasSolutionWithThreeOperatorsWorker(result, computed * remainingValues.head, remainingValues.tail) ||
    hasSolutionWithThreeOperatorsWorker(result, concat(computed, remainingValues.head), remainingValues.tail)
  }
}

def hasSolutionWithThreeOperators(equation: Equation): Boolean = {
  equation.values match {
    case first :: tail => hasSolutionWithThreeOperatorsWorker(equation.result, first, tail)
    case _             => false
  }
}

def resolveStar2(input: String): Long = {
  val equations = parse(input)
  equations
    .filter(hasSolutionWithThreeOperators)
    .map(_.result)
    .sum
}

// ------------------------------------------------------------------------------

object Puzzle07Test extends ZIOSpecDefault {
  import zio.nio.file.Path
  import helpers.Helpers.*
  val day  = getClass.getName.replaceAll(""".*Puzzle(\d+)Test.*""", "day$1")
  def spec = suite(s"puzzle $day")(
    test("star#1") {
      for {
        exampleInput1 <- fileContent(Path(s"data/$day/example-1.txt"))
        exampleResult1 = resolveStar1(exampleInput1)
        puzzleInput   <- fileContent(Path(s"data/$day/puzzle-1.txt"))
        puzzleResult   = resolveStar1(puzzleInput)
      } yield assertTrue(
        exampleResult1 == 3749L,
        puzzleResult == 20665830408335L
      )
    },
    test("star#2") {
      for {
        exampleInput1 <- fileContent(Path(s"data/$day/example-1.txt"))
        exampleResult1 = resolveStar2(exampleInput1)
        puzzleInput   <- fileContent(Path(s"data/$day/puzzle-1.txt"))
        puzzleResult   = resolveStar2(puzzleInput)
      } yield assertTrue(
        exampleResult1 == 11387L,
        puzzleResult == 354060705047464L
      )
    }
  ) @@ timed @@ sequential
}
