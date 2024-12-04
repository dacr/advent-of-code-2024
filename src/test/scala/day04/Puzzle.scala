package day04

import zio.*
import zio.test.*
import zio.test.TestAspect.*

import scala.annotation.tailrec
import scala.math.*

// ------------------------------------------------------------------------------
case class Coord(x: Int, y: Int) {
  def up        = copy(y = y - 1)
  def down      = copy(y = y + 1)
  def left      = copy(x = x - 1)
  def right     = copy(x = x + 1)
  def upLeft    = copy(x = x - 1, y = y - 1)
  def upRight   = copy(x = x + 1, y = y - 1)
  def downLeft  = copy(x = x - 1, y = y + 1)
  def downRight = copy(x = x + 1, y = y + 1)
}
case class Cell(letter: Char) extends AnyVal

val moves: List[Coord => Coord] = List(
  coord => coord.right,
  coord => coord.left,
  coord => coord.down,
  coord => coord.up,
  coord => coord.upLeft,
  coord => coord.downRight,
  coord => coord.downLeft,
  coord => coord.upRight
)

// ------------------------------------------------------------------------------
def parse(input: String): Map[Coord, Cell] = {
  input.trim
    .split("\\n")
    .zipWithIndex
    .flatMap { case (line, y) =>
      line.zipWithIndex.map { case (c, x) =>
        (Coord(x, y), Cell(c))
      }
    }
    .toMap
}

@tailrec
def checkFor(word: String, current: Coord, move: Coord => Coord, matrix: Map[Coord, Cell]): Boolean =
  matrix.get(current) match {
    case _ if word.isEmpty =>
      true

    case Some(cell) if word.head == cell.letter =>
      checkFor(word.tail, move(current), move, matrix)

    case _ => false
  }

// ------------------------------------------------------------------------------

def resolveStar1(input: String): Int = {
  val matrix = parse(input)

  matrix.keys.toList
    .map(coord => moves.count(move => checkFor("XMAS", coord, move, matrix)))
    .sum
}

// ------------------------------------------------------------------------------

def checkForX(coord: Coord, matrix: Map[Coord, Cell]): Boolean = {
  matrix.get(coord).exists(_.letter == 'A') && (
    (
      matrix.get(coord.upLeft).exists(_.letter == 'M') &&
        matrix.get(coord.downRight).exists(_.letter == 'S')
    ) ||
      (
        matrix.get(coord.upLeft).exists(_.letter == 'S') &&
          matrix.get(coord.downRight).exists(_.letter == 'M')
      )
  )
  &&
  (
    (
      matrix.get(coord.upRight).exists(_.letter == 'M') &&
        matrix.get(coord.downLeft).exists(_.letter == 'S')
    ) ||
      (
        matrix.get(coord.upRight).exists(_.letter == 'S') &&
          matrix.get(coord.downLeft).exists(_.letter == 'M')
      )
  )
}

def resolveStar2(input: String): Int = {
  val matrix = parse(input)
  matrix.keys.toList.count(coord => checkForX(coord, matrix))
}

// ------------------------------------------------------------------------------

object Puzzle04Test extends ZIOSpecDefault {
  import zio.nio.file.Path
  import helpers.Helpers.*
  val day  = getClass.getName.replaceAll(""".*Puzzle(\d+)Test.*""", "day$1")
  def spec = suite(s"puzzle $day")(
    test("star#1") {
      for {
        exampleInput0 <- fileContent(Path(s"data/$day/example-0.txt"))
        exampleResult0 = resolveStar1(exampleInput0)
        exampleInput1 <- fileContent(Path(s"data/$day/example-1.txt"))
        exampleResult1 = resolveStar1(exampleInput1)
        puzzleInput   <- fileContent(Path(s"data/$day/puzzle-1.txt"))
        puzzleResult   = resolveStar1(puzzleInput)
      } yield assertTrue(
        exampleResult0 == 4,
        exampleResult1 == 18,
        puzzleResult == 2551
      )
    },
    test("star#2") {
      for {
        exampleInput1 <- fileContent(Path(s"data/$day/example-1.txt"))
        exampleResult1 = resolveStar2(exampleInput1)
        puzzleInput   <- fileContent(Path(s"data/$day/puzzle-1.txt"))
        puzzleResult   = resolveStar2(puzzleInput)
      } yield assertTrue(
        exampleResult1 == 9,
        puzzleResult == 1985
      )
    }
  ) @@ timed @@ sequential
}
