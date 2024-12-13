package day13

import zio.test.*
import zio.test.TestAspect.*

import scala.jdk.StreamConverters.*

// ------------------------------------------------------------------------------
case class Entry(a: (x: Long, y: Long), b: (x: Long, y: Long), prize: (x: Long, y: Long))

// ------------------------------------------------------------------------------

def parseEntry(input: String): Entry = {
  val nums  = "\\d+".r
  val lines =
    input
      .split("\n", 3)
      .map(nums.findAllIn)
      .map(_.map(_.toLong).toList)
      .collect({ case x :: y :: Nil => (x = x, y = y) })

  val a = lines(0)
  val b = lines(1)
  val p = lines(2)

  Entry(a, b, p)
}

def parse(input: String): List[Entry] = {
  input.trim
    .split("\n\n")
    .map(parseEntry)
    .toList
}

// ------------------------------------------------------------------------------
def winnable(entry: Entry): Option[Long] = {
  val (pX, pY)   = entry.prize
  val (aX, aY)   = entry.a
  val (bX, bY)   = entry.b
  val aFactorMax = math.max(pX / aX, pX / bX) + 1
  val bFactorMax = math.max(pY / aY, pY / bY) + 1
  val results    =
    LazyList
      .iterate(1L)(_ + 1L)
      .takeWhile(_ <= aFactorMax)
      .flatMap(af =>
        LazyList
          .iterate(math.max((pX - af * aX) / bX, pY - af * aY) / bY)(_ + 1L)
          .takeWhile(_ <= bFactorMax)
          .map(bf => (af, bf, af * aX + bf * bX, af * aY + bf * bY))
          .dropWhile((af, bf, x, y) => x < pX && y < pY)
          .take(1)
          .filter((af, bf, x, y) => x == pX && y == pY)
          .map((af, bf, x, y) => af * 3 + bf * 1)
      )

  results.minOption
}

def resolveStar1(cells: List[Entry]): Long = {
  cells
    .flatMap(winnable)
    .sum
}

// ------------------------------------------------------------------------------

def winnable2(entry: Entry): Option[Long] = {
  val (aX, aY) = entry.a
  val (bX, bY) = entry.b
  val (pX, pY) = entry.prize
  val bf       = (aX * pY - aY * pX) / (aX * bY - aY * bX)
  val af       = (pX - bX * bf) / aX
  if (af * aX + bf * bX == pX && af * aY + bf * bY == pY) Some(af * 3 + bf)
  else None
}

def resolveStar2(cells: List[Entry]): Long = {
  val toAdd = 10000000000000L
  cells
    .map(c => c.copy(prize = (x = c.prize.x + toAdd, y = c.prize.y + toAdd)))
    .flatMap(winnable2)
    .sum
}

// ------------------------------------------------------------------------------

object Puzzle13Test extends ZIOSpecDefault {
  import zio.nio.file.Path
  import helpers.Helpers.*
  val day  = getClass.getName.replaceAll(""".*Puzzle(\d+)Test.*""", "day$1")
  def spec = suite(s"puzzle $day")(
    test("star#1") {
      for {
        exampleInput1 <- fileContent(Path(s"data/$day/example-1.txt")).map(parse)
        puzzleInput   <- fileContent(Path(s"data/$day/puzzle-1.txt")).map(parse)
      } yield assertTrue(
        resolveStar1(exampleInput1) == 480,
        resolveStar1(puzzleInput) == 28059
      )
    },
    test("star#2") {
      for {
        exampleInput1 <- fileContent(Path(s"data/$day/example-1.txt")).map(parse)
        puzzleInput   <- fileContent(Path(s"data/$day/puzzle-1.txt")).map(parse)
      } yield assertTrue(
        resolveStar2(exampleInput1) > 100,
        resolveStar2(puzzleInput) == 102255878088512L
      )
    }
  ) @@ timed @@ sequential
}
