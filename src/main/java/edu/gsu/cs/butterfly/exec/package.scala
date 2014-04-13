package edu.gsu.cs.butterfly

import java.io.{FileWriter, File}
import net.sourceforge.argparse4j.inf.ArgumentParserException
import net.sourceforge.argparse4j.ArgumentParsers.newArgumentParser


/**
 * Created with IntelliJ IDEA.
 * User: aartsiomenka1
 * Date: 11/22/13
 * Time: 2:27 PM
 * Package object hiding all implementation issues.
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
  val SENSITIVE = "sensitive"
  val KGEM_IN_FILENAME = "aligned_reads.fas"
  val ALIGNED_SAM_FILENAME = "reads.sam"
  val HAPLOTYPES_CLEANED = "haplotypes_cleaned.fas"

  private var output_dir: File = null
  private var reads_file: File = null
  private var reference_file: File = null
  private var k = 0


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
  
  def param_key(str: String) = KEY_PREFIX + str

  def param_metavar(str: String) = str.toUpperCase

  def parseArguments(args: Array[String]) = {
    val parser = newArgumentParser(BUTTERFLY)
      .defaultHelp(true)
      .description("Clustering procedure wrapper Butterfly-1.0")

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
