package day14

import zio.test.*
import zio.test.TestAspect.*

import scala.jdk.StreamConverters.*
import scala.util.chaining.*
import scala.collection.parallel.CollectionConverters._

// ------------------------------------------------------------------------------
type Vec = (x: Long, y: Long)
case class Robot(pos: Vec, vel: Vec)

// ------------------------------------------------------------------------------

def parseEntry(input: String): Robot = {
  val nums  = """-?\d+""".r
  val parts =
    nums
      .findAllIn(input)
      .map(_.toLong)
      .sliding(2, 2)
      .map(_.toList)
      .collect({ case x :: y :: Nil => (x = x, y = y) })
      .toList

  val pos = parts.head
  val vel = parts.last

  Robot(pos, vel)
}

def parse(input: String): List[Robot] = {
  input.trim
    .lines()
    .toScala(List)
    .map(parseEntry)
}

// ------------------------------------------------------------------------------
def move(width: Long, height: Long, elapsed: Long)(robot: Robot): Robot = {
  val (x, y)   = robot.pos
  val (vx, vy) = robot.vel

  val nx =
    if (vx >= 0) (x + vx * elapsed)   % width
    else (x + (width + vx) * elapsed) % width

  val ny =
    if (vy >= 0) (y + vy * elapsed)    % height
    else (y + (height + vy) * elapsed) % height

  robot.copy(pos = (nx, ny))
}

def simulate(robots: List[Robot], width: Long, height: Long, elapsed: Long): List[Robot] = {
  if (elapsed % 500_000L == 0L) println(elapsed)
  robots.map(move(width, height, elapsed))
}

def show(robots: List[Robot], width: Long, height: Long): Unit = {
  0.until(height.toInt).foreach { y =>
    0.until(width.toInt).foreach { x =>
      val robot = robots.count(r => (r.pos.x == x && r.pos.y == y))
      if (robot == 0) print(".")
      else print(robot)
    }
    println()
  }
}

def pos2quadrant(pos: Vec, width: Long, height: Long): Option[Int] = {
  val xmid = width / 2
  val ymid = height / 2
  if (pos.x < xmid && pos.y < ymid) Some(0)
  else if (pos.x > xmid && pos.y < ymid) Some(1)
  else if (pos.x < xmid && pos.y > ymid) Some(2)
  else if (pos.x > xmid && pos.y > ymid) Some(3)
  else None
}

def safetyFactor(robots: List[Robot], width: Long, height: Long): Long = {
  robots
    .groupBy(r => pos2quadrant(r.pos, width, height))
    .collect { case (Some(quadrant), robots) => quadrant -> robots.size }
    .values
    .reduce(_ * _)
}

def resolveStar1(robots: List[Robot], width: Long, height: Long): Long = {
  val result = simulate(robots, width, height, 100)
  show(result, width, height)
  safetyFactor(result, width, height)
}

// ------------------------------------------------------------------------------

def symetric(robots: List[Robot], width: Long, height: Long): Boolean = {
  robots
    .filter(_.pos.x < width / 2)
    .forall(robot => robots.exists(r => r.pos.y == robot.pos.y && r.pos.x == width - 1 - robot.pos.x))
}

def resolveStar2(robots: List[Robot], width: Long, height: Long): Long = {
  LazyList
    .from(0)
    .map(n => n -> simulate(robots, width, height, n))
    .find((n, robots) => symetric(robots, width, height))
    .map((n, robots) => {show(robots, width, height) ; n})
    .getOrElse(0)
}

// ------------------------------------------------------------------------------

object Puzzle14Test extends ZIOSpecDefault {
  import zio.nio.file.Path
  import helpers.Helpers.*
  val day  = getClass.getName.replaceAll(""".*Puzzle(\d+)Test.*""", "day$1")
  def spec = suite(s"puzzle $day")(
    test("star#1") {
      for {
        exampleInput1 <- fileContent(Path(s"data/$day/example-1.txt")).map(parse)
        puzzleInput   <- fileContent(Path(s"data/$day/puzzle-1.txt")).map(parse)
      } yield assertTrue(
        resolveStar1(exampleInput1, 11, 7) == 12,
        resolveStar1(puzzleInput, 101, 103) == 218619324
      )
    },
    test("star#2") {
      for {
        puzzleInput <- fileContent(Path(s"data/$day/puzzle-1.txt")).map(parse)
      } yield assertTrue(
        resolveStar2(puzzleInput, 101, 103) == 0
      )
    }
  ) @@ timed @@ sequential
}
