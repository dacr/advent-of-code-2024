package day06

import zio.*
import zio.test.*
import zio.test.TestAspect.*

import scala.annotation.tailrec
import scala.math.*
import scala.jdk.StreamConverters.*
import scala.collection.parallel.CollectionConverters.*
import scala.collection.parallel.ForkJoinTaskSupport

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
      line.zipWithIndex.map { case (content, x) =>
        (Coord(x, y), Cell(content, content == '#'))
      }
    }
    .toMap
}

def findStart(area: Map[Coord, Cell]): Option[(Coord, Mover)] = {
  area
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
      val cell  = area.get(coord)
      if (coord == current) print(mover.id)
      else if (coords.contains(coord)) print('X')
      else if (cell.exists(_.obstacle)) print(cell.get.content)
      else print('.')
    }
    println()
  }
  println()
}

def walk1(area: Map[Coord, Cell], startCoord: Coord, startMover: Mover): Set[Coord] = {
  @tailrec
  def worker(current: Coord, mover: Mover, visited: Set[Coord]): Set[Coord] = {
    // show(area, current, mover, visited)
    val nextCoord = mover.go(current)
    if (startMover == mover && startCoord == current && visited.contains(current)) visited
    else
      area.get(nextCoord) match {
        case None => visited + current

        case Some(cell) if cell.obstacle =>
          worker(current, Mover.turns(mover), visited)

        case Some(cell) =>
          worker(nextCoord, mover, visited + current)
      }
  }
  worker(startCoord, startMover, Set())
}

def resolveStar1(input: String): Int = {
  val area = parse(input)
  findStart(area) match {
    case None               => -1
    case Some(start, mover) =>
      walk1(area, start, mover).size
  }
}

// ------------------------------------------------------------------------------

def walk2(area: Map[Coord, Cell], startCoord: Coord, startMover: Mover): Int = {
  @tailrec
  def isLoopWorker(area: Map[Coord, Cell], current: (Coord, Mover), visited: Set[(Coord, Mover)]): Boolean = {
    if (visited.contains(current)) true
    else {
      val (coord,mover) = current
      val nextCoord = mover.go(coord)
      area.get(nextCoord) match {
        case None => false

        case Some(nextCell) if nextCell.obstacle =>
          isLoopWorker(area, (coord, Mover.turns(mover)), visited + current)

        case Some(_) =>
          isLoopWorker(area, (nextCoord, mover), visited + current)
      }
    }
  }
  val anObstacle = Cell('O', true)
  area
    .filterNot((coord, cell) => cell.obstacle || coord == startCoord)
    .toList
    .par
    .count((coord, cell) => isLoopWorker(area.updated(coord, anObstacle), (startCoord, startMover), Set()))
}

def resolveStar2(input: String): Int = {
  val area = parse(input)
  findStart(area) match {
    case None               => -1
    case Some(start, mover) =>
      walk2(area, start, mover)
  }
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
        puzzleResult < 1587,
        puzzleResult == 1586
      )
    }
  ) @@ timed @@ sequential
}
