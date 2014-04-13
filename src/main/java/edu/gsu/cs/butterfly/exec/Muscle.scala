package edu.gsu.cs.butterfly.exec

import java.io.File
import org.biojava3.core.sequence.DNASequence
import org.biojava3.core.sequence.io.{FastaReaderHelper, FastaWriterHelper}
import collection.JavaConversions._
import sys.process._

/**
 * Wrapper object for Muscle
 */
object Muscle {
  private val MUSCLE = "muscle"
  private val IN = "-in"
  private val OUT = "-out"
  private val QUIET = "-quiet"
  private val DIAGS = "-diags"
  private val MAXITERS = "-maxiters"
  private val FASTA = ".fas"

  private def generateTmpFile = {
    val f = File.createTempFile(MUSCLE, FASTA)
    f.deleteOnExit()
    f
  }

  private def writeTmpFasta(seqs: Iterable[DNASequence]) = {
    val tmp_file = generateTmpFile
    FastaWriterHelper.writeNucleotideSequence(tmp_file, seqs)
    tmp_file
  }

  private def prepareInputArgs(in: File, out: File, maxiters: Int) = {
    if (in == null || !in.exists() || !in.canRead)
      throw new Exception("Incorrect input for MUSCLE")
    val args = Array[String](MUSCLE, IN, in.getAbsolutePath,
      QUIET, DIAGS, MAXITERS, maxiters.toString, OUT, out.getAbsolutePath)
    args
  }

  private def getProcessLogger: FileProcessLogger = {
    val f = generateTmpFile
    val log = ProcessLogger.apply(f)
    log
  }

  /**
   * Perform MSA with Muscle
   * @param in
   * File path to file with input sequences
   * @param maxiters
   * Muscle argument 1, 2 or 3 (3 most accurate)
   * @return
   * Collection of sequences aligned with muscle
   */
  def muscle(in: File, maxiters: Int): Iterable[DNASequence] = {
    if (!testMuscle) throw new Exception("Muscle executable is not available")
    val log = getProcessLogger
    try {
      val out = generateTmpFile
      val args = prepareInputArgs(in, out, maxiters)
      val p = Process(args)
      if (p ! log == 0) {
        FastaReaderHelper.readFastaDNASequence(out).values()
      } else {
        throw new Exception("Muscle execution error")
      }
    } catch {
      case e: Exception => {
        e.printStackTrace()
        None
      }
    } finally {
      log.close
    }
  }

  /**
   * Perform MSA with Muscle
   * @param seqs
   * Collection of sequences to align
   * @param maxiters
   * Muscle argument 1, 2 or 3 (3 most accurate)
   * @return
   * Collection of sequences aligned with muscle
   */
  def muscle(seqs: Iterable[DNASequence], maxiters: Int = 3): Iterable[DNASequence] = {
    val in = writeTmpFasta(seqs)
    muscle(in, maxiters)
  }

  /**
   * Test if muscle is in PATH or
   * proper link is in working directory
   * @return
   * True if executable is valid
   */
  def testMuscle: Boolean = {
    val log = getProcessLogger
    try {
      MUSCLE ! log
      true
    } catch {
      case e: Exception => {
        e.printStackTrace()
        false
      }
    } finally {
      log.close
    }
  }
}