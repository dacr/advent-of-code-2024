package day06

import zio.*
import zio.test.*
import zio.test.TestAspect.*

import scala.annotation.tailrec
import scala.math.*
import scala.jdk.StreamConverters._

// ------------------------------------------------------------------------------
case class Coord(x: Int, y: Int) {
  def up    = copy(y = y - 1)
  def down  = copy(y = y + 1)
  def left  = copy(x = x - 1)
  def right = copy(x = x + 1)
}

case class Cell(content: Char, obstacle: Boolean)

enum Mover(val id: Char, val go: Coord => Coord) {
  case Up    extends Mover('^', _.up)
  case Right extends Mover('>', _.right)
  case Down  extends Mover('v', _.down)
  case Left  extends Mover('<', _.left)
}

object Mover {
  val turns = Map(
    Mover.Up    -> Mover.Right,
    Mover.Right -> Mover.Down,
    Mover.Down  -> Mover.Left,
    Mover.Left  -> Mover.Up
  )
}

// ------------------------------------------------------------------------------

def parse(input: String): Map[Coord, Cell] = {
  input.trim
    .lines()
    .toScala(List)
    .zipWithIndex
    .flatMap { case (line, y) =>
      line.zipWithIndex.map { case (c, x) =>
        (Coord(x, y), Cell(c, c == '#'))
      }
    }
    .toMap
}

def findStart(coordToCell: Map[Coord, Cell]): Option[(Coord, Mover)] = {
  coordToCell
    .find((coord, cell) => "<>v^".contains(cell.content))
    .flatMap((coord, cell) =>
      Mover.values
        .find(_.id == cell.content)
        .map(mover => coord -> mover)
    )
}

// ------------------------------------------------------------------------------

def show(area: Map[Coord, Cell], current: Coord, mover: Mover, coords: Set[Coord]): Unit = {
  0.to(area.keys.maxBy(c => c.y).y).foreach { y =>
    0.to(area.keys.maxBy(c => c.x).x).foreach { x =>
      val coord = Coord(x, y)
      if (coord == current) print(mover.id)
      else if (coords.contains(coord)) print('X')
      else if (area.get(coord).exists(_.obstacle)) print('#')
      else print('.')
    }
    println()
  }
  println()
}

@tailrec
def walk(area: Map[Coord, Cell], startCoord: Coord, startMover: Mover, current: Coord, mover: Mover, visited: Set[Coord]): Set[Coord] = {
  //show(area, current, mover, visited)
  val nextCoord = mover.go(current)
  if (startMover.id == mover.id && startCoord == current && visited.contains(current)) visited
  else
    area.get(nextCoord) match {
      case None => visited + current

      case Some(cell) if cell.obstacle =>
        walk(area, startCoord, startMover, current, Mover.turns(mover), visited)

      case Some(cell) =>
        walk(area, startCoord, startMover, nextCoord, mover, visited + current)
    }
}

def resolveStar1(input: String): Int = {
  val area = parse(input)
  findStart(area) match {
    case None               => -1
    case Some(start, mover) =>
      walk(area, start, mover, start, mover, Set()).size
  }
}

// ------------------------------------------------------------------------------

def resolveStar2(input: String): Int = {
  0
}

// ------------------------------------------------------------------------------

object Puzzle06Test extends ZIOSpecDefault {
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
        exampleResult1 == 41,
        puzzleResult == 4776
      )
    },
    test("star#2") {
      for {
        exampleInput1 <- fileContent(Path(s"data/$day/example-1.txt"))
        exampleResult1 = resolveStar2(exampleInput1)
        puzzleInput   <- fileContent(Path(s"data/$day/puzzle-1.txt"))
        puzzleResult   = resolveStar2(puzzleInput)
      } yield assertTrue(
        exampleResult1 == 6,
        puzzleResult == 4716
      )
    }
  ) @@ timed @@ sequential
}
