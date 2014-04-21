package edu.gsu.cs.butterfly

import java.io.File
import net.sourceforge.argparse4j.inf.ArgumentParserException
import net.sourceforge.argparse4j.ArgumentParsers.newArgumentParser
import org.biojava3.core.sequence.io.{FastaWriterHelper, FastaReaderHelper}
import collection.JavaConversions._
import edu.gsu.cs.kgem.exec.log
import edu.gsu.cs.butterfly.model.HammingDistanceSeedFinder
import HammingDistanceSeedFinder.{getKClusters, distance}
import org.biojava3.core.sequence.DNASequence

/**
 * Package object hiding all implementation issues.
 */
package object exec {
  private val KEY_PREFIX = "-"
  private val K_PARAM = "k"
  private val REF_PARAM = "g"
  private val INPUT_PARAM = "in"
  private val OUTPUT_PARAM = "o"
  private val USER_DIR = "user.dir"
  protected[exec] val DEFAULT_OUT = new File(System.getProperty(USER_DIR) + "/haplotypes.fas")
  protected[exec] val BUTTERFLY = "Butterfly v.%s amplicon error correction based on kGEM"
  protected[exec] val NAME = "butterfly"

  private var output_file: File = null
  private var reads_file: File = null
  private var reference_file: File = null
  private var k = 0

  def test = {
    println(distance("AAACCTG", "AAC"))
    println(distance("ATG", "AATCCGC"))
  }


  def findMostLikelySeq = {
    log("Reading input files...")
    val reads = FastaReaderHelper.readFastaDNASequence(reads_file).values()
    val ref = FastaReaderHelper.readFastaDNASequence(reference_file).values().head
    log("Files were read.")
    val map = reads.map(x => (x, reads.view.filter(y => distance(x, y) <=k)))
  }

  def extractByLen(min: Int = 0, max: Int = Int.MaxValue) = {
    log("Reading input files...")
    val reads = FastaReaderHelper.readFastaDNASequence(reads_file).values()
    val ref = FastaReaderHelper.readFastaDNASequence(reference_file).values().head
    log("Files were read.")
    log("Size: %d".format(reads.size))
    val freads = reads.filter(x => x.getLength >= min && x.getLength <= max).toList
    log("Filtered size: %d".format(freads.size))
    log("Max lens: %s".format(freads.map(_.getLength).sorted.takeRight(10)))
    FastaWriterHelper.writeNucleotideSequence(new File(output_file.getAbsolutePath), freads)
  }

  def split = {
    log("Reading input files...")
    val reads = FastaReaderHelper.readFastaDNASequence(reads_file).values()
    val ref = FastaReaderHelper.readFastaDNASequence(reference_file).values().head
    log("Files were read.")
    val freads = reads.filter(_.getLength > 0.95 * ref.getLength)
    log("Filtered reads: %d".format(freads.size))
    val in = freads.filter(x => distance(x, ref) < k)
    val out = freads.filter(x => distance(x, ref) >= k)
    log("In size: %d out size: %d".format(in.size, out.size))
    FastaWriterHelper.writeNucleotideSequence(new File(output_file.getAbsolutePath + "_in.fas"), in)
    FastaWriterHelper.writeNucleotideSequence(new File(output_file.getAbsolutePath + "_out.fas"), out)
  }

  def run = {
    log("Reading input files...")
    val reads = FastaReaderHelper.readFastaDNASequence(reads_file).values()
    val ref = FastaReaderHelper.readFastaDNASequence(reference_file).values().head
    log("%s".format(reads.map(_.getLength).toSet))
    log(ref.getOriginalHeader)
    log("Files were read.")

    log("Clustering started...")
    val clusters = getKClusters(ref, reads, k)
    var t = 0

    log("Clustering finished.")
    for (i <- clusters){
      val rad = i._2.view.map(x => distance(x, i._1)).max
      log("Size: %d -> radius: %d".format(i._2.size, rad))
      //       val aligned_reads = muscle(i._2).toList
      //       val hapls = edu.gsu.cs.kgem.exec.executeKgem(aligned_reads, 5, 3, 0.04, 0.01)
      //       val fasta_hapls = hapls.sortBy(-_.freq).map(s => {
      //         val dna = new DNASequence(s.toIntegralString.replaceAll("-",""))
      //         dna.setOriginalHeader("h_freq=%.5f".format(s.freq))
      //         dna
      //       })
      //i._1.setOriginalHeader("%s_size_%d".format(i._1.getOriginalHeader, i._2.size))
      FastaWriterHelper.writeNucleotideSequence(new File(output_file.getAbsolutePath + t +"size_" + i._2.size + ".fas"), List(i._1))
      val reads = i._2.map(x => {
        val seq = new DNASequence(x.getSequenceAsString.replaceAll("N", "").replaceAll("-",""))
        seq.setOriginalHeader(x.getOriginalHeader)
        seq
      })
      FastaWriterHelper.writeNucleotideSequence(new File(output_file.getAbsolutePath + t + ".fas"), reads)
      t += 1
    }
    log("Finished!")
  }

  private def handleError(mes: String) {
    System.err.println(mes)
    System.exit(-1)
  }

  def setupOutputDir(dir: File) = {
    if (!dir.exists())
      if (!dir.mkdir())
        handleError("Output directory does not exist and could not be created!")
    if (!dir.isDirectory)
      handleError("Output directory is not a directory!")
    dir
  }
  
  private def param_key(str: String) = KEY_PREFIX + str

  private def param_metavar(str: String) = str.toUpperCase

  def parseArguments(args: Array[String]) = {
    val parser = newArgumentParser(NAME)
      .defaultHelp(true)
      .description(BUTTERFLY.format(Main.getClass.getPackage.getImplementationVersion))

    parser.addArgument(param_key(K_PARAM))
      .dest(K_PARAM)
      .metavar(param_metavar(K_PARAM))
      .`type`(classOf[Integer])
      .help("Number of initial seeds (clusters).")

    parser.addArgument(param_key(REF_PARAM))
      .dest(REF_PARAM)
      .metavar(param_metavar(REF_PARAM))
      .`type`(classOf[File])
      .help("Path to the file with references.")

    parser.addArgument(param_key(INPUT_PARAM))
      .dest(INPUT_PARAM)
      .metavar(param_metavar(INPUT_PARAM))
      .`type`(classOf[File])
      .help("Path to the file with amplicon reads.")

    parser.addArgument(param_key(OUTPUT_PARAM))
      .dest(OUTPUT_PARAM)
      .metavar(param_metavar(OUTPUT_PARAM))
      .`type`(classOf[File])
      .help("Output file path.")

    try {
      val n = parser.parseArgs(args)
      if (n.get(K_PARAM) == null || n.get(INPUT_PARAM) == null) throw new ArgumentParserException(
      "Mandatory parameters are missing! -k and -in", parser)
      k = n.getInt(K_PARAM)
      if (k < 0) throw new ArgumentParserException(
        "Number of samples in pool is mandatory (Parameter: %s > 0)".format(K_PARAM), parser)
      if (!n.get(INPUT_PARAM).isInstanceOf[File])
        throw new ArgumentParserException(
          "Filename with reads is wrong (Parameter: %s)".format(INPUT_PARAM), parser)
      if (!n.get(REF_PARAM).isInstanceOf[File])
        throw new ArgumentParserException(
          "Filename with references is wrong (Parameter: %s)".format(REF_PARAM), parser)
      if (!n.get(OUTPUT_PARAM).isInstanceOf[File])
        output_file = DEFAULT_OUT
      else
        output_file = n.get(OUTPUT_PARAM).asInstanceOf[File]

      setupOutputDir(output_file.getAbsoluteFile.getParentFile)

      reads_file = n.get(INPUT_PARAM).asInstanceOf[File]

      reference_file = n.get(REF_PARAM).asInstanceOf[File]
    } catch {
      case e: ArgumentParserException => {
        parser.handleError(e)
        System.exit(1)
      }
    }
  }
}
