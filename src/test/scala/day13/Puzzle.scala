// topographic map
package day13

import zio.test.*
import zio.test.TestAspect.*

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.jdk.StreamConverters.*

// ------------------------------------------------------------------------------
case class Entry(a: (x: Int, y: Int), b: (x: Int, y: Int), prize: (x: Int, y: Int))

// ------------------------------------------------------------------------------

def parseEntry(input: String): Entry = {
  val nums  = "\\d+".r
  val lines =
    input
      .split("\n", 3)
      .map(nums.findAllIn)
      .map(_.map(_.toInt).toList)
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
def winnable(entry: Entry): Option[Int] = {
  val (pX, pY)   = entry.prize
  val (aX, aY)   = entry.a
  val (bX, bY)   = entry.b
  val aFactorMax = math.max(pX / aX, pX / bX) + 1
  val bFactorMax = math.max(pY / aY, pY / bY) + 1
  val results =
  LazyList
    .from(0)
    .take(aFactorMax)
    .flatMap(af =>
      LazyList
        .from(0)
        .take(bFactorMax)
        .map(bf => (af, bf))
    )
    .filter((af, bf) => (af * aX + bf*bX == pX) && (af * aY + bf*bY) == pY)
    .map((af, bf) => af * 3 + bf * 1)
    .toList

  results
    .minOption
}

def resolveStar1(cells: List[Entry]): Long = {
  cells
    .flatMap(winnable)
    .sum
}

// ------------------------------------------------------------------------------

def resolveStar2(cells: List[Entry]): Long = {
  0
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
        resolveStar2(exampleInput1) == 0,
        resolveStar2(puzzleInput) == 0 // > 39200 >770831
      )
    }
  ) @@ timed @@ sequential
}
