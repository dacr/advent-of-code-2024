package day08

import zio.*
import zio.test.*
import zio.test.TestAspect.*

import scala.jdk.StreamConverters.*

// ------------------------------------------------------------------------------
type Frequency = Char
case class Coord(x: Int, y: Int)
case class Antenna(coord: Coord, frequency: Frequency)
case class Area(width: Int, height: Int, antennas: List[Antenna])

def parse(input: String): Area = {
  val lines    = input.trim.lines().toScala(List)
  val antennas = lines.zipWithIndex.flatMap { case (line, y) =>
    line.zipWithIndex.collect {
      case (frequency, x) if frequency != '.' =>
        Antenna(Coord(x, y), frequency)
    }
  }
  Area(lines.head.length, lines.length, antennas)
}

def show(area: Area, antinodes: Set[Coord]) = {
  0.until(area.height).foreach { y =>
    0.until(area.width).foreach { x =>
      val coord = Coord(x, y)
      val freq  = area.antennas.find(_.coord == coord).map(_.frequency)
      val node  = antinodes.find(_ == coord)
      val char  = node.map(_ => '#').getOrElse(freq.getOrElse('.'))
      print(char)
    }
    println()
  }
}

// ------------------------------------------------------------------------------

def antinodes1(selected: List[Antenna]): List[Coord] = {
  selected match {
    case List(a, b) =>
      val dx = b.coord.x - a.coord.x
      val dy = b.coord.y - a.coord.y
      List(
        Coord(a.coord.x - dx, a.coord.y - dy),
        Coord(b.coord.x + dx, b.coord.y + dy)
      )

    case _ => Nil
  }
}

def resolveStar1(input: String): Long = {
  val area = parse(input)

    area.antennas
      .groupBy(_.frequency)
      .toList
      .flatMap { (freq, antennas) =>
        antennas
          .combinations(2)
          .toList
          .flatMap(selected => antinodes1(selected))
          .filter(antinode => antinode.x >= 0 && antinode.x < area.width)
          .filter(antinode => antinode.y >= 0 && antinode.y < area.height)
      }
      .toSet
      .size
}

// ------------------------------------------------------------------------------

def antinodes2(area: Area, selected: List[Antenna]): Set[Coord] = {
  selected match {
    case List(a, b) =>
      val dx = b.coord.x - a.coord.x
      val dy = b.coord.y - a.coord.y

      val growing = LazyList
        .iterate((a.coord.x, a.coord.y))((x, y) => (x + dx, y + dy))
        .takeWhile((x, y) => x >= 0 && x < area.width && y >= 0 && y < area.height)
        .map((x, y) => Coord(x, y))
        .toSet

      val decreasing = LazyList
        .iterate((a.coord.x, a.coord.y))((x, y) => (x - dx, y - dy))
        .takeWhile((x, y) => x >= 0 && x < area.width && y >= 0 && y < area.height)
        .map((x, y) => Coord(x, y))
        .toSet

      growing ++ decreasing

    case _ => Set.empty
  }
}

def resolveStar2(input: String): Long = {
  val area = parse(input)

  area.antennas
    .groupBy(_.frequency)
    .toList
    .flatMap { (freq, antennas) =>
      antennas
        .combinations(2)
        .toList
        .flatMap(selected => antinodes2(area, selected))
    }
    .toSet
    .size
}

// ------------------------------------------------------------------------------

object Puzzle08Test extends ZIOSpecDefault {
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
        exampleResult1 == 14,
        puzzleResult < 320,
        puzzleResult != 302,
        puzzleResult == 301
      )
    },
    test("star#2") {
      for {
        exampleInput0 <- fileContent(Path(s"data/$day/example-0.txt"))
        exampleResult0 = resolveStar2(exampleInput0)
        exampleInput1 <- fileContent(Path(s"data/$day/example-1.txt"))
        exampleResult1 = resolveStar2(exampleInput1)
        puzzleInput   <- fileContent(Path(s"data/$day/puzzle-1.txt"))
        puzzleResult   = resolveStar2(puzzleInput)
      } yield assertTrue(
        exampleResult0 == 9,
        exampleResult1 == 34,
        puzzleResult == 1019
      )
    }
  ) @@ timed @@ sequential
}
