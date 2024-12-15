package day15

import sourcecode.Text.generate
import zio.test.*
import zio.test.TestAspect.*

import scala.jdk.StreamConverters.*
import scala.util.chaining.*
import scala.collection.parallel.CollectionConverters.*

// ------------------------------------------------------------------------------

type Coord = (x: Int, y: Int)
type Wall  = '#'
type Box   = 'O'
type Robot = '@'
type Cell  = Wall | Box | Robot
type Move  = Coord => Coord

def up(coord: Coord): Coord    = (coord.x, coord.y - 1)
def down(coord: Coord): Coord  = (coord.x, coord.y + 1)
def left(coord: Coord): Coord  = (coord.x - 1, coord.y)
def right(coord: Coord): Coord = (coord.x + 1, coord.y)

def neighbors(coord: Coord): List[Coord] = List(up(coord), down(coord), left(coord), right(coord))
def gps(coord: Coord): Int               = coord.y * 100 + coord.x

// ------------------------------------------------------------------------------

def parse(input: String): (area: Map[Coord, Cell], moves: List[Move]) = {
  val Array(area, rawMoves) = input.trim.split("\n\n", 2)

  val lines                    = area.lines().toScala(List)
  val coords: Map[Coord, Cell] =
    lines.zipWithIndex
      .flatMap((line, y) =>
        line.zipWithIndex.collect {
          case (wall: Wall, x)   => (x, y) -> wall
          case (box: Box, x)     => (x, y) -> box
          case (robot: Robot, x) => (x, y) -> robot
        }
      )
      .toMap

  val moves: List[Move] = rawMoves.collect {
    case '<' => left
    case '>' => right
    case '^' => up
    case 'v' => down
  }.toList

  (coords, moves)
}

// ------------------------------------------------------------------------------

def moveBoxes(pos: Coord, boxes: Set[Coord], walls: Set[Coord], move: Move): (moved: Boolean, newBoxes: Set[Coord]) = {
  val newPos = move(pos)
  if (walls.contains(newPos)) (false, boxes)
  else if (!boxes.contains(newPos)) (true, boxes - pos + newPos)
  else {
    moveBoxes(newPos, boxes, walls, move) match {
      case (false, newBoxes) => (false, boxes)
      case (true, newBoxes)  => (true, newBoxes - pos + newPos)
    }
  }
}

def resolveStar1(input: String): Long = {
  val (cells: Map[Coord, Cell], moves: List[Move]) = parse(input)

  val walls = cells.collect { case (coord, _: Wall) => coord }.toSet
  val boxes = cells.collect { case (coord, _: Box) => coord }.toSet
  val robot = cells.collect { case (coord, _: Robot) => coord }.head

  val (lastPos, movedBoxes) = moves.foldLeft(robot -> boxes) { case ((pos, boxes), move) =>
    val newPos = move(pos)
    if (walls.contains(newPos)) (pos, boxes)
    else if (!boxes.contains(newPos)) (newPos, boxes)
    else
      moveBoxes(newPos, boxes, walls, move) match {
        case (true, newBoxes)  => (newPos, newBoxes)
        case (false, newBoxes) => (pos, boxes)
      }
  }

  movedBoxes.map(gps).sum
}

// ------------------------------------------------------------------------------
def resolveStar2(input: String): Int = {
  0
}

// ------------------------------------------------------------------------------

object Puzzle15Test extends ZIOSpecDefault {
  import zio.nio.file.Path
  import helpers.Helpers.*
  val day  = getClass.getName.replaceAll(""".*Puzzle(\d+)Test.*""", "day$1")
  def spec = suite(s"puzzle $day")(
    test("star#1") {
      for {
        exampleInput1 <- fileContent(Path(s"data/$day/example-1.txt"))
        exampleInput2 <- fileContent(Path(s"data/$day/example-2.txt"))
        puzzleInput   <- fileContent(Path(s"data/$day/puzzle-1.txt"))
      } yield assertTrue(
        resolveStar1(exampleInput1) == 10092,
        resolveStar1(exampleInput2) == 2028,
        resolveStar1(puzzleInput) == 1505963
      )
    },
    test("star#2") {
      for {
        exampleInput1 <- fileContent(Path(s"data/$day/example-1.txt"))
        exampleInput2 <- fileContent(Path(s"data/$day/example-1.txt"))
        puzzleInput   <- fileContent(Path(s"data/$day/puzzle-1.txt"))
      } yield assertTrue(
        resolveStar1(exampleInput1) == -1,
        resolveStar1(exampleInput2) == -1,
        resolveStar2(puzzleInput) == -1
      )
    }
  ) @@ timed @@ sequential
}
