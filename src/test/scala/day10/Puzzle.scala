// topographic map
package day10

import zio.*
import zio.test.*
import zio.test.TestAspect.*

import scala.annotation.tailrec
import scala.jdk.StreamConverters.*

case class Coord(x: Int, y: Int) {
  def up: Coord    = copy(y = y - 1)
  def down: Coord  = copy(y = y + 1)
  def left: Coord  = copy(x = x - 1)
  def right: Coord = copy(x = x + 1)

  def neighbors: List[Coord] = List(up, down, left, right)
}

case class Cell(coord: Coord, height: Int)
// ------------------------------------------------------------------------------

def parse(input: String): List[Cell] = {
  input.trim
    .lines()
    .toScala(List)
    .zipWithIndex
    .flatMap((line, y) =>
      line.zipWithIndex
        .collect { case (ch, x) if ch.isDigit => Cell(Coord(x, y), ch.asDigit) }
    )
}

// ------------------------------------------------------------------------------
def search(topographicMap: Map[Coord, Cell], current: Cell): List[Cell] = {
  if (current.height == 0) current :: Nil
  else {
    current.coord.neighbors
      .flatMap(topographicMap.get)
      .filter(_.height == current.height - 1)
      .flatMap(subCell => search(topographicMap, subCell))
  }
}

def score(tuples: List[(Cell, Cell)]): Int = {
  tuples.distinct
    .groupBy((trailhead, target) => trailhead)
    .map((trailhead, targets) => targets.size)
    .sum
}

def resolveStar1(input: String): Long = {
  val cells          = parse(input)
  val topographicMap = cells.groupBy(_.coord).map((k, v) => k -> v.head)
  val targets        = cells.filter(_.height == 9)
  score(
    targets.flatMap(target =>
      search(topographicMap, target)
        .map(trailhead => (trailhead, target))
    )
  )
}

// ------------------------------------------------------------------------------

def resolveStar2(input: String): Long = {
  val cells          = parse(input)
  val topographicMap = cells.groupBy(_.coord).map((k, v) => k -> v.head)
  val targets        = cells.filter(_.height == 9)
  targets
    .flatMap(target => search(topographicMap, target).map(trailhead => (trailhead, target)))
    .size
}

// ------------------------------------------------------------------------------

object Puzzle10Test extends ZIOSpecDefault {
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
        exampleInput2 <- fileContent(Path(s"data/$day/example-2.txt"))
        exampleResult2 = resolveStar1(exampleInput2)
        exampleInput3 <- fileContent(Path(s"data/$day/example-3.txt"))
        exampleResult3 = resolveStar1(exampleInput3)
        exampleInput4 <- fileContent(Path(s"data/$day/example-4.txt"))
        exampleResult4 = resolveStar1(exampleInput4)
        puzzleInput   <- fileContent(Path(s"data/$day/puzzle-1.txt"))
        puzzleResult   = resolveStar1(puzzleInput)
      } yield assertTrue(
        exampleResult0 == 1,
        exampleResult1 == 36,
        exampleResult2 == 2,
        exampleResult3 == 4,
        exampleResult4 == 3,
        puzzleResult == 659
      )
    },
    test("star#2") {
      for {
        exampleInput1 <- fileContent(Path(s"data/$day/example-1.txt"))
        exampleResult1 = resolveStar2(exampleInput1)
        puzzleInput   <- fileContent(Path(s"data/$day/puzzle-1.txt"))
        puzzleResult   = resolveStar2(puzzleInput)
      } yield assertTrue(
        exampleResult1 == 81,
        puzzleResult == 1463
      )
    }
  ) @@ timed @@ sequential
}
