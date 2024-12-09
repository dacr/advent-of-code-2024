package day09

import zio.*
import zio.test.*
import zio.test.TestAspect.*

import scala.annotation.tailrec
import scala.jdk.StreamConverters.*

// ------------------------------------------------------------------------------
case class Block(len: Int, owner: Option[Int]) {
  def isEmpty = owner.isEmpty
  def isUsed  = owner.isDefined
}

def parse(input: String): Vector[Block] = {
  val (blocks, nextId) =
    input.trim.zipWithIndex
      .foldLeft((Vector[Block](), 0)) {
        case ((blocks, id), (n, index)) if n.asDigit == 0 =>
          (blocks, id)

        case ((blocks, id), (n, index)) if blocks.isEmpty =>
          (blocks :+ Block(n.asDigit, Some(id)), id + 1)

        case ((blocks, id), (n, index)) if index % 2 == 0 =>
          (blocks :+ Block(n.asDigit, Some(id)), id + 1)

        case ((blocks, id), (n, index)) =>
          (blocks :+ Block(n.asDigit, None), id)
      }

  blocks
}

def show(blocks: Vector[Block]): Unit = {
  blocks.foreach { block =>
    block.owner match {
      case Some(id) => print(s"$id" * block.len)
      case None     => print("." * block.len)
    }
  }
  println()
}

// ------------------------------------------------------------------------------
@tailrec
def compactBlocks(blocks: Vector[Block]): Vector[Block] = {
  val firstEmpty = blocks.indexWhere(_.owner.isEmpty)
  val lastUsed   = blocks.lastIndexWhere(_.owner.isDefined)

  if (firstEmpty < 0 || firstEmpty > lastUsed) blocks
  else {
    val usedBlock  = blocks(lastUsed)
    val emptyBlock = blocks(firstEmpty)
    val rightPatch = if (usedBlock.len == 1) Nil else usedBlock.copy(len = usedBlock.len - 1) :: Nil
    val leftPatch  = emptyBlock.copy(owner = usedBlock.owner, len = 1) :: (
      if (emptyBlock.len == 1) Nil
      else emptyBlock.copy(len = emptyBlock.len - 1) :: Nil
    )

    val patched =
      blocks
        .patch(lastUsed, rightPatch, 1)
        .patch(firstEmpty, leftPatch, 1)
    compactBlocks(patched)
  }
}

def checksum(blocks: Vector[Block]): Long = {
  val (checksum, _) = blocks
    .foldLeft((0L, 0L)) {
      case ((checksum, index), Block(len, None))        => (checksum, index + len)
      case ((checksum, index), Block(len, Some(owner))) =>
        val subChecksum = index.until(index + len).map(i => i * owner).sum
        (checksum + subChecksum, index + len)
    }
  checksum
}

def resolveStar1(input: String): Long = {
  val blocks    = parse(input)
  val compacted = compactBlocks(blocks)
  checksum(compacted)
}

// ------------------------------------------------------------------------------

def compactFiles(blocks: Vector[Block]): Vector[Block] = {
  val tryToMove =
    blocks
      .filter(_.isUsed)
      .sortBy(b => -b.owner.get)

  val compacted =
    tryToMove.foldLeft(compactFreeBlocks(blocks, true)) { (compacting, file) =>
      val usedIndex  = compacting.indexWhere(b => b.owner == file.owner)
      val availIndex = compacting.indexWhere(b => b.isEmpty && b.len >= file.len)
      if (availIndex < 0 || availIndex > usedIndex) compacting
      else {
        val freeBlock  = compacting(availIndex)
        val leftPatch  = file :: (
          if (freeBlock.len == file.len) Nil
          else Block(freeBlock.len - file.len, None) :: Nil
        )
        val rightPatch = Block(len = file.len, owner = None)

        val patched =
          compacting
            .patch(usedIndex, rightPatch :: Nil, 1)
            .patch(availIndex, leftPatch, 1)

        compactFreeBlocks(patched, false)
      }
    }

  compacted
}

def compactFreeBlocks(blocks: Vector[Block], all: Boolean): Vector[Block] = {
  val doubleFreeIndex =
    blocks
      .sliding(2, 1)
      .indexWhere(x => x.forall(_.isEmpty))
  if (doubleFreeIndex == -1) blocks
  else {
    val first   = blocks(doubleFreeIndex)
    val second  = blocks(doubleFreeIndex + 1)
    val patched = blocks.patch(doubleFreeIndex, Block(first.len + second.len, None) :: Nil, 2)
    if (all) compactFreeBlocks(patched, all)
    else patched
  }
}

def resolveStar2(input: String): Long = {
  val blocks    = parse(input)
  val compacted = compactFiles(blocks)
  if (blocks.size < 100) show(compacted)
  checksum(compacted)
}

// ------------------------------------------------------------------------------

object Puzzle09Test extends ZIOSpecDefault {
  import zio.nio.file.Path
  import helpers.Helpers.*
  val day  = getClass.getName.replaceAll(""".*Puzzle(\d+)Test.*""", "day$1")
  def spec = suite(s"puzzle $day")(
    test("star#1") {
      for {
        exampleInput0 <- ZIO.succeed("12345")
        exampleResult0 = resolveStar1(exampleInput0)
        exampleInput1  = "90909"
        exampleResult1 = resolveStar1(exampleInput1)
        exampleInput2 <- fileContent(Path(s"data/$day/example-1.txt"))
        exampleResult2 = resolveStar1(exampleInput2)
        puzzleInput   <- fileContent(Path(s"data/$day/puzzle-1.txt"))
        puzzleResult   = resolveStar1(puzzleInput)
      } yield assertTrue(
        exampleResult0 == 60,
        exampleResult1 == 513,
        exampleResult2 == 1928,
        puzzleResult > 1990928337L,
        puzzleResult > 4176699140049L,
        puzzleResult == 6310675819476L
      )
    },
    test("star#2") {
      for {
        exampleInput1 <- fileContent(Path(s"data/$day/example-1.txt"))
        exampleResult1 = resolveStar2(exampleInput1)
        puzzleInput   <- fileContent(Path(s"data/$day/puzzle-1.txt"))
        puzzleResult   = resolveStar2(puzzleInput)
      } yield assertTrue(
        exampleResult1 == 2858,
        puzzleResult < 6336159513240L,
        puzzleResult == 6335972980679L
      )
    }
  ) @@ timed @@ sequential
}
