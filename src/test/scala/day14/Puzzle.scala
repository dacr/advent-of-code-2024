package day14

import zio.test.*
import zio.test.TestAspect.*

import scala.jdk.StreamConverters.*
import scala.util.chaining.*
import scala.collection.parallel.CollectionConverters._

// ------------------------------------------------------------------------------
type Vec = (x: Long, y: Long)
case class Robot(x: Long, y: Long, vx: Long, vy: Long)

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

  Robot(pos.x, pos.y, vel.x, vel.y)
}

def parse(input: String): List[Robot] = {
  input.trim
    .lines()
    .toScala(List)
    .map(parseEntry)
}

// ------------------------------------------------------------------------------
def move(width: Long, height: Long, elapsed: Long)(robot: Robot): Robot = {
  val nx = (robot.x + (width + robot.vx) * elapsed)  % width
  val ny = (robot.y + (height + robot.vy) * elapsed) % height
  robot.copy(x = nx, y = ny)
}

def simulate(robots: List[Robot], width: Long, height: Long, elapsed: Long): List[Robot] = {
  if (elapsed % 500_000L == 0L) println(elapsed)
  robots.map(move(width, height, elapsed))
}

def show(robots: List[Robot], width: Long, height: Long): Unit = {
  0.until(height.toInt).foreach { y =>
    0.until(width.toInt).foreach { x =>
      val robot = robots.count(r => r.x == x && r.y == y)
      if (robot == 0) print(".")
      else print(robot)
    }
    println()
  }
}

def pos2quadrant(x: Long, y: Long, width: Long, height: Long): Option[Int] = {
  val xmid = width / 2
  val ymid = height / 2
  if (x < xmid && y < ymid) Some(0)
  else if (x > xmid && y < ymid) Some(1)
  else if (x < xmid && y > ymid) Some(2)
  else if (x > xmid && y > ymid) Some(3)
  else None
}

def safetyFactor(robots: List[Robot], width: Long, height: Long): Long = {
  robots
    .groupBy(r => pos2quadrant(r.x, r.y, width, height))
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

//def symetric(robots: List[Robot], width: Long, height: Long): Boolean = {
//  robots
//    .filter(_.x < width / 2)
//    .forall(robot => robots.exists(r => r.y == robot.y && r.x == width - 1 - robot.x))
//}
//
//def resolveStar2(robots: List[Robot], width: Long, height: Long): Long = {
//  LazyList
//    //.iterate(0L)(_ + 1L)
//    .iterate(3_006_500_000L)(_ + 1L)
//    .map(n => n -> simulate(robots, width, height, n))
//    .find((n, robots) => symetric(robots, width, height))
//    .map((n, robots) => { show(robots, width, height); n })
//    .getOrElse(0L)
//}
//
//def symetric(robots: List[Robot], width: Long, height: Long): Boolean = {
//  robots
//    .filter(_.x < width / 2)
//    .forall(robot => robots.exists(r => r.y == robot.y && r.x == width - 1 - robot.x))
//}

//def isSymetric(xs: Array[Long], ys: Array[Long], width: Long): Boolean = {
//  var i        = 0
//  var symetric = true
//  while (symetric && i < xs.length) {
//    val x     = xs(i)
//    val y     = ys(i)
//    val ox    = width - 1 - x
//    var j     = 0
//    var found = false
//    while (!found && j < xs.length) {
//      if (ox == xs(j) && y == ys(j)) found = true
//      j = j + 1
//    }
//    if (!found) symetric = false
//    i = i + 1
//  }
//  symetric
//}
//
//def resolveStar2(robots: List[Robot], width: Long, height: Long): Long = {
//  val xs      = robots.map(_.x).toArray
//  val ys      = robots.map(_.y).toArray
//  val vxs     = robots.map(_.vx).toArray
//  val vys     = robots.map(_.vy).toArray
//  var elapsed = 6_635_500_000L
//  //var elapsed = 1_087_500_000L
//  var found   = Option.empty[Long]
//  while (found.isEmpty) {
//    if (elapsed % 500_000L == 0L) println(elapsed)
//    var i = 0
//    while (i < xs.length) {
//      xs(i) = (xs(i) + (width + vxs(i)) * elapsed)  % width
//      ys(i) = (ys(i) + (height + vys(i)) * elapsed) % height
//      i = i + 1
//    }
//    if (isSymetric(xs, ys, width)) found = Some(elapsed)
//    elapsed += 1
//  }
//  found.get
//}

def resolveStar2(robots: List[Robot], width: Long, height: Long): Int = {
  LazyList
    .from(1)
    .map(n => n -> simulate(robots, width, height, n))
    .find((n, robots) =>
      robots
        .groupBy(r => pos2quadrant(r.x, r.y, width, height))
        .collect { case (Some(quadrant), robots) => quadrant -> robots }
        .exists((q, r) => r.size >= robots.size / 2)
    )
    .map((n, robots) => n)
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
        //isSymetric(Array(0, 4, 1, 3), Array(0, 0, 1, 1), 5L),
        resolveStar2(puzzleInput, 101, 103) == 6446
      )
    }
  ) @@ timed @@ sequential
}
