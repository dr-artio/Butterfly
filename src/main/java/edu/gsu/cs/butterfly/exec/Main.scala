package edu.gsu.cs.butterfly.exec

import org.biojava3.core.sequence.io.{FastaWriterHelper, FastaReaderHelper}
import java.io.File
import collection.JavaConversions._
import edu.gsu.cs.butterfly.exec.Muscle.muscle


/**
 * Butterfly tool main class
 * Tool is created for performing
 * k-medoid algorithm based on kGEM
 */
object Main {
   def main(args: Array[String]) = {
     val reads = FastaReaderHelper.readFastaDNASequence(new File("simPool2.fas")).values()
     FastaWriterHelper.writeNucleotideSequence(new File("aligned.fas"), muscle(reads, 1))
   }
}
