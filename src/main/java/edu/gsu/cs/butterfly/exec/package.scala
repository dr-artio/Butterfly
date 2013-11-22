package edu.gsu.cs.butterfly

import java.io.File
import net.sourceforge.argparse4j.inf.ArgumentParserException
import net.sourceforge.argparse4j.ArgumentParsers.newArgumentParser
import edu.gsu.cs.align.io.SAMParser


/**
 * Created with IntelliJ IDEA.
 * User: aartsiomenka1
 * Date: 11/22/13
 * Time: 2:27 PM
 * To change this template use File | Settings | File Templates.
 */
package object exec {
  val KEY_PREFIX = "-"
  val K = "k"
  val REFERENCE = "g"
  val READS = "i"
  val OUTPUT_DIR = "o"
  val C = "c"
  val BUTTERFLY = "Butterfly"
  val NO_HASHING = "noHashing"
  val KGEM_IN_FILENAME = "aligned_reads.fas"
  val ALIGNED_SAM_FILENAME = "reads.sam"
  val HAPLOTYPES_CLEANED = "haplotypes_cleaned.fas"

  private var output_dir: File = null
  private var reads_file: File = null
  private var reference_file: File = null
  private var k = 0
  private var kgem_args: Array[String] = null
  private var indelfixer_args: Array[String] = null
  private var shared_args: Array[String] = null

  def runErif = {
    if (shared_args != null)
      edu.gsu.cs.align.exec.Main(indelfixer_args ++ shared_args)
    else
      edu.gsu.cs.align.exec.Main(indelfixer_args)
  }

  def runKgem = {
    runKgem(kgem_args)
  }

  def runKgem(args: Array[String]) = {
    if (shared_args != null)
      edu.gsu.cs.kgem.exec.Main(args ++ shared_args)
    else
      edu.gsu.cs.kgem.exec.Main(args)
  }

  /**
   * Process joint list of parameters for
   * Butterfly(kGEM + ERIF). Partition all
   * args on three groups: IndelFixer Specific,
   * kGEM specific and shared arguments.
   * @param args
   *             Joint list of arguments
   */
  def preProcessArgs(args: Array[String]) = {
    kgem_args = new Array[String](4)
    shared_args = new Array[String](2)
    indelfixer_args = new Array[String](args.length)
    args.copyToArray(indelfixer_args)

    processSharedArgs(args)
    processKgemArgs(args)
    indelfixer_args = indelfixer_args.filter(_ != null)

    println("Done")
  }

  def processFirstKgemArgs = {
    val new_args = new Array[String](kgem_args.length)
    kgem_args.copyToArray(new_args)

    new_args(1) = getNumberOfSubtypes

    new_args
  }

  private def getNumberOfSubtypes = {
    val records = SAMParser.readSAMFile(output_dir.getAbsolutePath + File.separator + ALIGNED_SAM_FILENAME)
    records.map(_.getReferenceName).toSet.size
  }

  private def processKgemArgs(args: Array[String]) {
    kgem_args(0) = output_dir.getAbsolutePath + File.separator + KGEM_IN_FILENAME
    val ak = args.indexOf(param_key(K))
    if (ak < 0) {
      handleError("Number of samples in pool is mandatory (Parameter: %s > 0)".format(K))
    } else {
      kgem_args(1) = args(ak + 1)
      indelfixer_args(ak) = null
      indelfixer_args(ak + 1) = null
    }
  }

  private def handleError(mes: String) {
    System.err.println(mes)
    System.exit(-1)
  }

  private def processSharedArgs(args: Array[String]) {
    val o = args.indexOf(param_key(OUTPUT_DIR))
    if (o < 0) {
      output_dir = setupOutputDir(new File("./"))
      shared_args = null
    } else {
      shared_args(0) = param_key(OUTPUT_DIR)
      val s = args(o + 1)
      shared_args(1) = if (s.endsWith(File.separator)) s else s + File.separator
      output_dir = setupOutputDir(new File(shared_args(1)))
      indelfixer_args(o) = null
      indelfixer_args(o + 1) = null
    }
  }

  def setupOutputDir(dir: File) = {
    if (!dir.exists())
      if (!dir.mkdir())
        handleError("Output directory does not exist and could not be created!")
    if (!dir.isDirectory)
      handleError("Output directory is not a directory!")
    dir
  }
  
  def param_key(str: String) = KEY_PREFIX + str

  def param_metavar(str: String) = str.toUpperCase

  def parseArguments(args: Array[String]) = {
    val parser = newArgumentParser(BUTTERFLY)
      .defaultHelp(true)
      .description("Clustering procedure wrapper.")

    parser.addArgument(param_key(K))
      .dest(K)
      .setDefault[Integer](-1)
      .metavar(param_metavar(K))
      .`type`(classOf[Integer])
      .help("Number of samples in the pool.")

    parser.addArgument(param_key(REFERENCE))
      .dest(REFERENCE)
      .metavar(param_metavar(REFERENCE))
      .`type`(classOf[File])
      .help("Path to the file with references.")

    parser.addArgument(param_key(READS))
      .dest(READS)
      .metavar(param_metavar(READS))
      .`type`(classOf[File])
      .help("Path to the file with reads.")

    parser.addArgument(param_key(OUTPUT_DIR))
      .dest(OUTPUT_DIR)
      .metavar(param_metavar(OUTPUT_DIR))
      .`type`(classOf[File])
      .help("Path to the file with references.")

    try {
      val n = parser.parseArgs(args)
      k = n.getInt(K)
      if (k < 0) throw new ArgumentParserException("Number of samples in pool is mandatory (Parameter: %s > 0)".format(K), parser)
      if (!n.get(READS).isInstanceOf[File])
        throw new ArgumentParserException("Filename with reads is wrong (Parameter: %s)".format(READS), parser)
      if (!n.get(REFERENCE).isInstanceOf[File])
        throw new ArgumentParserException("Filename with references is wrong (Parameter: %s)".format(REFERENCE), parser)
      if (!n.get(OUTPUT_DIR).isInstanceOf[File])
        output_dir = new File("./")
      else
        output_dir = n.get(OUTPUT_DIR).asInstanceOf[File]

      reads_file = n.get(READS).asInstanceOf[File]

      reference_file = n.get(REFERENCE).asInstanceOf[File]
    } catch {
      case e: ArgumentParserException => {
        parser.handleError(e)
        System.exit(1)
      }
    }
  }
}
