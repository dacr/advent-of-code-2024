package day05

import zio.*
import zio.test.*
import zio.test.TestAspect.*

import scala.annotation.tailrec
import scala.math.*
import scala.jdk.StreamConverters._

// ------------------------------------------------------------------------------
case class PageOrderingRule(before: Int, after: Int)
type PageOrderingRules  = List[PageOrderingRule]
type PagesToProduce     = Array[Int]
type PagesToProduceList = List[PagesToProduce]

def parse(input: String): (PageOrderingRules, PagesToProduceList) = {
  val ordRE = """(\d+)[|](\d+)""".r
  input.trim.split("""\n\n""", 2) match {
    case Array(orders, produces) =>
      val pageOrderingRules =
        orders
          .lines()
          .toScala(List)
          .collect { case ordRE(a, b) => PageOrderingRule(a.toInt, b.toInt) }

      val pagesToProduces =
        produces
          .lines()
          .toScala(List)
          .map(_.split(",").map(_.toInt).toArray)

      (pageOrderingRules, pagesToProduces)

    case _ => (Nil, Nil)
  }
}

// ------------------------------------------------------------------------------

def checkOrdering(pages: PagesToProduce, rules: PageOrderingRules): Boolean = {
  rules.forall { rule =>
    val before = pages.indexOf(rule.before)
    val after  = pages.indexOf(rule.after)
    (before < after) || (before == -1) || (after == -1)
  }
}

def getCenterPage(pages: PagesToProduce): Int =
  pages(pages.size / 2)

def resolveStar1(input: String): Int = {
  val (orderings, pagesToProduceList) = parse(input)
  pagesToProduceList
    .filter(checkOrdering(_, orderings))
    .map(getCenterPage)
    .sum
}

// ------------------------------------------------------------------------------

def hasBadOrdering(pages: PagesToProduce, rules: PageOrderingRules): Boolean = {
  rules.exists { rule =>
    val before = pages.indexOf(rule.before)
    val after  = pages.indexOf(rule.after)
    before > after && after != -1 && before != -1
  }
}

def fixOrdering(pages: PagesToProduce, rules: PageOrderingRules): PagesToProduce = {
  val fixed = rules.foldLeft(pages) { case (pages, rule) =>
    val before = pages.indexOf(rule.before)
    val after  = pages.indexOf(rule.after)
    if (before > after && before != -1 && after != -1) {
      val tmp = pages(before)
      pages.updated(before, pages(after)).updated(after, tmp)
    } else {
      pages
    }
  }
  if (fixed sameElements pages) fixed
  else {
    fixOrdering(fixed, rules)
  }
}

def resolveStar2(input: String): Int = {
  val (orderings, pagesToProduceList) = parse(input)
  pagesToProduceList
    .filter(hasBadOrdering(_, orderings))
    .map(fixOrdering(_, orderings))
    .map(getCenterPage)
    .sum
}

// ------------------------------------------------------------------------------

object Puzzle05Test extends ZIOSpecDefault {
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
        exampleResult1 == 143,
        puzzleResult == 5732
      )
    },
    test("star#2") {
      for {
        exampleInput1 <- fileContent(Path(s"data/$day/example-1.txt"))
        exampleResult1 = resolveStar2(exampleInput1)
        puzzleInput   <- fileContent(Path(s"data/$day/puzzle-1.txt"))
        puzzleResult   = resolveStar2(puzzleInput)
      } yield assertTrue(
        exampleResult1 == 123,
        puzzleResult == 4716
      )
    }
  ) @@ timed @@ sequential
}
