package edu.gsu.cs.butterfly

import java.io.File
import net.sourceforge.argparse4j.inf.ArgumentParserException
import net.sourceforge.argparse4j.ArgumentParsers.newArgumentParser
import org.biojava3.core.sequence.io.{FastaWriterHelper, FastaReaderHelper}
import collection.JavaConversions._
import edu.gsu.cs.kgem.exec.log
import edu.gsu.cs.butterfly.model.{UkkonenSeedFinder, HammingDistanceSeedFinder}
import HammingDistanceSeedFinder.distance
import org.biojava3.core.sequence.DNASequence
import scala.collection.mutable.ListBuffer
import scala.collection.parallel.{ForkJoinTaskSupport, TaskSupport}
import scala.concurrent.forkjoin.ForkJoinPool

/**
 * Package object hiding all implementation issues.
 */
package object exec {
  private val KEY_PREFIX = "-"
  private val K_PARAM = "k"
  private val NUMPROC_PARAM = "numproc"
  private val INPUT_PARAM = "in"
  private val OUTPUT_PARAM = "o"
  private val USER_DIR = "user.dir"
  protected[exec] val DEFAULT_OUT = new File(System.getProperty(USER_DIR) + "/haplotypes.fas")
  protected[exec] val BUTTERFLY = "Butterfly v.%s amplicon error correction based on kGEM"
  protected[exec] val NAME = "butterfly"

  private var output_file: File = null
  private var reads_file: File = null
  private var numproc: TaskSupport = null
  private var k = 0

  def getNumProc = {
    numproc
  }

  def test = {
    println(distance("AAACCTG", "AAC"))
    println(distance("ATG", "AATCCGC"))
  }


  def findMostLikelySeq = {
    log("Reading input files...")
    val reads = FastaReaderHelper.readFastaDNASequence(reads_file).values()
    log("Files were read.")
    val map = reads.map(x => (x, reads.view.filter(y => distance(x, y) <=k)))
  }

  def extractByLen(min: Int = 0, max: Int = Int.MaxValue) = {
    log("Reading input files...")
    val reads = FastaReaderHelper.readFastaDNASequence(reads_file).values()
    log("Files were read.")
    log("Size: %d".format(reads.size))
    val freads = reads.filter(x => x.getLength >= min && x.getLength <= max).toList
    log("Filtered size: %d".format(freads.size))
    log("Max lens: %s".format(freads.map(_.getLength).sorted.takeRight(10)))
    FastaWriterHelper.writeNucleotideSequence(new File(output_file.getAbsolutePath), freads)
  }

//  def split = {
//    log("Reading input files...")
//    val reads = FastaReaderHelper.readFastaDNASequence(reads_file).values()
//    val ref = FastaReaderHelper.readFastaDNASequence(reference_file).values().head
//    log("Files were read.")
//    val freads = reads.filter(_.getLength > 0.95 * ref.getLength)
//    log("Filtered reads: %d".format(freads.size))
//    val in = freads.filter(x => distance(x, ref) < k)
//    val out = freads.filter(x => distance(x, ref) >= k)
//    log("In size: %d out size: %d".format(in.size, out.size))
//    FastaWriterHelper.writeNucleotideSequence(new File(output_file.getAbsolutePath + "_in.fas"), in)
//    FastaWriterHelper.writeNucleotideSequence(new File(output_file.getAbsolutePath + "_out.fas"), out)
//  }

//  def run1 = {
//    log("Reading input files...")
//    val reads = FastaReaderHelper.readFastaDNASequence(reads_file).values()
//    val ref = FastaReaderHelper.readFastaDNASequence(reference_file).values().head
//    log("%s".format(reads.map(_.getLength).toSet))
//    log(ref.getOriginalHeader)
//    log("Files were read.")
//
//    UkkonenSeedFinder.outErrorRates(ref, reads)
//  }

  def run = {
    log("Reading input files...")
    val reads = FastaReaderHelper.readFastaDNASequence(reads_file).values()
    log("Files were read.")

    log("Clustering started...")
    //val clusters = getKClusters(ref, reads, k)
    val clusters = HammingDistanceSeedFinder.getClusters(reads, k)
    var t = 0

    log("Clustering finished.")
    val out = new File(output_file.getAbsolutePath)
    val haplotypes = new ListBuffer[(Double, DNASequence)]()
    val gl_size = reads.size.toDouble

    for (i <- clusters){
      val rad = i._2.view.map(x => distance(x, i._1)).max
      log("Size: %d -> radius: %d".format(i._2.size, rad))

      val hapl = new DNASequence(i._1.getSequenceAsString.replaceAll("N", "").replaceAll("-",""))
      val f = (i._2.size.toDouble / gl_size) * 100
      hapl.setOriginalHeader("haplotype%d_f=%.2f%%".format(t, f))

      haplotypes += ((f, hapl))

      t += 1
    }
    FastaWriterHelper.writeNucleotideSequence(out, haplotypes.sortBy(-_._1).map(_._2))
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

    parser.addArgument(param_key(NUMPROC_PARAM))
      .dest(NUMPROC_PARAM)
      .metavar(param_metavar(NUMPROC_PARAM))
      .`type`(classOf[Integer])
      .help("Number of threads for parallel execution.")

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
      var np = Runtime.getRuntime.availableProcessors()
      val n = parser.parseArgs(args)
      if (n.get(K_PARAM) == null || n.get(INPUT_PARAM) == null) throw new ArgumentParserException(
      "Mandatory parameters are missing! -k or -in", parser)
      k = n.getInt(K_PARAM)
      if (k < 0) throw new ArgumentParserException(
        "Number of samples in pool is mandatory (Parameter: %s > 0)".format(K_PARAM), parser)
      if (!n.get(INPUT_PARAM).isInstanceOf[File])
        throw new ArgumentParserException(
          "Filename with reads is wrong (Parameter: %s)".format(INPUT_PARAM), parser)
      if (n.get(NUMPROC_PARAM) == null || !n.get(NUMPROC_PARAM).isInstanceOf[Integer]) {
        log("Numproc is not set or incorrect. Default value is: %d".format(np))
      } else {
        np = n.getInt(NUMPROC_PARAM)
      }
      if (!n.get(OUTPUT_PARAM).isInstanceOf[File])
        output_file = DEFAULT_OUT
      else
        output_file = n.get(OUTPUT_PARAM).asInstanceOf[File]

      setupOutputDir(output_file.getAbsoluteFile.getParentFile)

      reads_file = n.get(INPUT_PARAM).asInstanceOf[File]

      numproc = new ForkJoinTaskSupport(new ForkJoinPool(np))
    } catch {
      case e: ArgumentParserException => {
        parser.handleError(e)
        System.exit(1)
      }
    }
  }
}
