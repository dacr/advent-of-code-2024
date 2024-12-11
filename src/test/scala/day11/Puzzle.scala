// topographic map
package day11

import zio.*
import zio.test.*
import zio.test.TestAspect.*

import scala.annotation.tailrec
import scala.collection.convert.ImplicitConversions.`map AsJavaMap`
import scala.jdk.StreamConverters.*

// ------------------------------------------------------------------------------
// 0 => 1
// even number of digits xxxxyyyy => xxxx yyyy
// odd number => odd * 2024

def compute(value: Long, blinking: Int): Long = {
  if (blinking == 0) 1
  else if (value == 0)
    compute(1, blinking - 1)
  else {
    val svalue = value.toString
    if (svalue.length % 2 == 1)
      compute(value * 2024, blinking - 1)
    else
      compute(svalue.take(svalue.length / 2).toLong, blinking - 1) +
        compute(svalue.drop(svalue.length / 2).toLong, blinking - 1)
  }
}

def resolveStar1(input: String, blinking: Int): Long = {
  input.trim
    .split(" ")
    .map(_.toLong)
    .map(stone => compute(stone, blinking))
    .sum
}

// ------------------------------------------------------------------------------
var cache = Map.empty[(Long, Int), Long]

def cached(value: Long, blinking: Int, orCompute: (Long, Int) => Long): Long = {
  val key = (value, blinking)
  cache.get(key) match {
    case Some(cachedValue) => cachedValue
    case None              =>
      val count = orCompute(value, blinking)
      cache = cache + (key -> count)
      count
  }
}

def compute2(value: Long, blinking: Int): Long = {
  if (cache.contains((value, blinking))) cache((value, blinking))
  else if (blinking == 0) 1
  else if (value == 0) {
    cached(1, blinking - 1, compute2)
  } else {
    val svalue = value.toString
    if (svalue.length % 2 == 1) {
      cached(value * 2024, blinking - 1, compute2)
    } else {
      cached(svalue.take(svalue.length / 2).toLong, blinking - 1, compute2) +
        cached(svalue.drop(svalue.length / 2).toLong, blinking - 1, compute2)
    }
  }
}

def resolveStar2(input: String, blinking: Int): Long = {
  input.trim
    .split(" ")
    .map(_.toLong)
    .map(stone => compute2(stone, blinking))
    .sum
}

// ------------------------------------------------------------------------------

object Puzzle10Test extends ZIOSpecDefault {
  import zio.nio.file.Path
  import helpers.Helpers.*
  val day  = getClass.getName.replaceAll(""".*Puzzle(\d+)Test.*""", "day$1")
  def spec = suite(s"puzzle $day")(
    test("star#1") {
      assertTrue(
        resolveStar1("0 1 10 99 999", 1) == 7,
        resolveStar1("125 17", 0) == 2,
        resolveStar1("125 17", 1) == 3,
        resolveStar1("125 17", 2) == 4,
        resolveStar1("125 17", 3) == 5,
        resolveStar1("125 17", 4) == 9,
        resolveStar1("125 17", 5) == 13,
        resolveStar1("125 17", 6) == 22,
        resolveStar1("125 17", 25) == 55312,
        resolveStar1("5 127 680267 39260 0 26 3553 5851995", 25) == 216042
      )
    },
    test("star#2") {
      assertTrue(
        resolveStar2("5 127 680267 39260 0 26 3553 5851995", 75) == 255758646442399L
      )
    }
  ) @@ timed @@ sequential
}
