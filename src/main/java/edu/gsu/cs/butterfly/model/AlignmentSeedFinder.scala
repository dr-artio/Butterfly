package edu.gsu.cs.butterfly.model

import java.util.logging.{Level, Logger}
import jaligner.matrix.{MatrixLoader, Matrix}
import jaligner.Sequence
import jaligner.SmithWatermanGotoh._

/**
 * Alignment distance implementation for
 * SeedFinder
 */
object AlignmentSeedFinder extends SeedFinder {
  /**
   * Static block just to disable logging
   * when call align method from jaligner
   */
  {
    val logger = Logger.getLogger(classOf[jaligner.SmithWatermanGotoh].getName)
    logger.getParent.setLevel(Level.OFF)
  }

  private val matrix: Matrix = MatrixLoader.load("EDNAFULL")
  private val gap_open = 10
  private val gap_extend = 4
  private val MATCH = '|'

  private def toSequence(str: String) = {
    new Sequence(str, "", "", Sequence.NUCLEIC)
  }

  def distance(arg1: String, arg2: String): Int = {
    val seq1 = toSequence(arg1)
    val seq2 = toSequence(arg2)
    val alignment = align(seq1, seq2, matrix, gap_open, gap_extend)
    val dist = alignment.getMarkupLine.count(_ != MATCH)
    return dist
  }
}
