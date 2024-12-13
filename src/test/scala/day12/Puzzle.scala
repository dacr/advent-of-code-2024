package day12

import zio.test.*
import zio.test.TestAspect.*

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.jdk.StreamConverters.*

// ------------------------------------------------------------------------------

case class Coord(x: Int, y: Int) {
  def up: Coord    = copy(y = y - 1)
  def down: Coord  = copy(y = y + 1)
  def left: Coord  = copy(x = x - 1)
  def right: Coord = copy(x = x + 1)

  def neighbors: List[Coord]   = List(up, down, left, right)
  def horizontals: List[Coord] = List(up, down)
  def verticals: List[Coord]   = List(left, right)
}

case class Cell(coord: Coord, kind: Char)

case class Region(cells: Set[Cell], kind: Char)

// ------------------------------------------------------------------------------

def parse(input: String): List[Cell] = {
  input.trim
    .lines()
    .toScala(List)
    .zipWithIndex
    .flatMap((line, y) =>
      line.zipWithIndex
        .collect((ch, x) => Cell(Coord(x, y), ch))
    )
}

// ------------------------------------------------------------------------------
def extractRegion(from: Cell, arrangement: Map[Coord, Cell]): Region = {
  var foundCells   = Set(from)
  var exploreQueue = Queue(from)
  while (exploreQueue.nonEmpty) {
    val (current, updatedQueue) = exploreQueue.dequeue
    exploreQueue = updatedQueue

    val newCells =
      current.coord.neighbors
        .flatMap(arrangement.get)
        .filter(_.kind == from.kind)
        .filterNot(foundCells)
    foundCells ++= newCells
    exploreQueue = exploreQueue.enqueueAll(newCells)
  }
  Region(foundCells, from.kind)
}

def extractRegions(cells: List[Cell]): List[Region] = {
  val arrangement = cells.groupBy(_.coord).map((coord, cells) => coord -> cells.head)
  cells.foldLeft(List.empty[Region]) { (regions, cell) =>
    if (regions.exists(_.cells.contains(cell))) regions
    else extractRegion(cell, arrangement) :: regions
  }
}

def score(region: Region): Long = {
  val coords    = region.cells.map(_.coord)
  val area      = coords.size
  val perimeter =
    coords.toList
      .flatMap(_.neighbors)
      .filterNot(coords)
      .size
  area * perimeter
}

// ------------------------------------------------------------------------------
def resolveStar1(cells: List[Cell]): Long = {
  val regions = extractRegions(cells)
  regions
    .map(score)
    .sum
}

// ------------------------------------------------------------------------------

def continuousCount(values: List[Int]): Int = {
  values.sliding(2, 1).foldLeft(1) {
    case (acc, List(a))                  => acc
    case (acc, List(a, b)) if a + 1 == b => acc
    case (acc, List(a, b))               => acc + 1
  }
}

def score2(region: Region): Int = {
  val coords = region.cells.map(_.coord)
  val area   = coords.size

  val aroundLefties  = coords.map(_.left).filterNot(coords)
  val aroundRighties = coords.map(_.right).filterNot(coords)
  val aroundUppies   = coords.map(_.up).filterNot(coords)
  val aroundDownies  = coords.map(_.down).filterNot(coords)

  val uppies   = aroundUppies.groupBy(_.y).toList.map((y, cs) => y -> cs.map(_.x).toList.sorted).sortBy((y, xs) => y)
  val downies  = aroundDownies.groupBy(_.y).toList.map((y, cs) => y -> cs.map(_.x).toList.sorted).sortBy((y, xs) => y)
  val lefties  = aroundLefties.groupBy(_.x).toList.map((x, cs) => x -> cs.map(_.y).toList.sorted).sortBy((x, ys) => x)
  val righties = aroundRighties.groupBy(_.x).toList.map((x, cs) => x -> cs.map(_.y).toList.sorted).sortBy((x, ys) => x)

  val contigusUppies   = uppies.map((y, xs) => continuousCount(xs)).sum
  val contigusDownies  = downies.map((y, xs) => continuousCount(xs)).sum
  val contigusLefties  = lefties.map((x, ys) => continuousCount(ys)).sum
  val contigusRighties = righties.map((x, ys) => continuousCount(ys)).sum

  val result = area * (contigusUppies + contigusDownies + contigusLefties + contigusRighties)

  result
}

def resolveStar2(cells: List[Cell]): Long = {
  val regions = extractRegions(cells)
  regions
    .map(score2)
    .sum
}

// ------------------------------------------------------------------------------

object Puzzle12Test extends ZIOSpecDefault {
  import zio.nio.file.Path
  import helpers.Helpers.*
  val day  = getClass.getName.replaceAll(""".*Puzzle(\d+)Test.*""", "day$1")
  def spec = suite(s"puzzle $day")(
    test("star#1") {
      for {
        exampleInput1 <- fileContent(Path(s"data/$day/example-1.txt")).map(parse)
        exampleInput2 <- fileContent(Path(s"data/$day/example-2.txt")).map(parse)
        exampleInput3 <- fileContent(Path(s"data/$day/example-3.txt")).map(parse)
        puzzleInput   <- fileContent(Path(s"data/$day/puzzle-1.txt")).map(parse)
      } yield assertTrue(
        extractRegions(exampleInput1).size == 5,
        extractRegions(exampleInput2).size == 5,
        extractRegions(exampleInput3).size == 11,
        resolveStar1(exampleInput1) == 140,
        resolveStar1(exampleInput2) == 772,
        resolveStar1(exampleInput3) == 1930,
        resolveStar1(puzzleInput) == 1433460
      )
    },
    test("star#2") {
      for {
        exampleInput1 <- fileContent(Path(s"data/$day/example-1.txt")).map(parse)
        exampleInput2 <- fileContent(Path(s"data/$day/example-2.txt")).map(parse)
        exampleInput3 <- fileContent(Path(s"data/$day/example-3.txt")).map(parse)
        exampleInput4 <- fileContent(Path(s"data/$day/example-4.txt")).map(parse)
        exampleInput5 <- fileContent(Path(s"data/$day/example-5.txt")).map(parse)
        puzzleInput   <- fileContent(Path(s"data/$day/puzzle-1.txt")).map(parse)
      } yield assertTrue(
        resolveStar2(exampleInput1) == 80,
        resolveStar2(exampleInput2) == 436,
        resolveStar2(exampleInput3) == 1206,
        resolveStar2(exampleInput4) == 236,
        resolveStar2(exampleInput5) == 368,
        resolveStar2(puzzleInput) == 855082 // > 39200 >770831
      )
    }
  ) @@ timed @@ sequential
}
