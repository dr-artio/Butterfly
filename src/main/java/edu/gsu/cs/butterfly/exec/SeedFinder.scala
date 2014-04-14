package edu.gsu.cs.butterfly.exec

import scala.collection.parallel.mutable.{ParMap, ParHashMap}
import org.biojava3.core.sequence.DNASequence
import java.util.logging.{Level, Logger}
import scala.collection.mutable.{HashMap, ListBuffer}
import edu.gsu.cs.kgem.exec.log

/**
 * Initial seed finder for clustering
 * using greedy approach. For full
 * functionality requires implementation
 * of distance method. Might be Edit
 * distance, alignment distance or
 * any distance defined on set of all
 * Strings
 */
trait SeedFinder {
  protected val cache = new HashMap[(String, String), Int]()

  def distance(arg1: String, arg2: String): Int
  def distance(seq1: DNASequence, seq2: DNASequence) : Int = {
    distance(seq1.getSequenceAsString, seq2.getSequenceAsString)
  }

  def getKSeeds(first: DNASequence, all: Iterable[DNASequence], k: Int) = {
    val seeds = new ListBuffer[DNASequence]()
    seeds += first
    log("Intitial distance map started...")
    val m = all.par.map(x => (x, distance(first, x))).seq.toMap
    log("Initial distance map computed.")
    val distanceMap = ParHashMap(m.toSeq: _*)
    while (seeds.size < k) {
      log("Iteration...")
      val next = distanceMap.maxBy(_._2)._1
      for (key <- distanceMap.keySet.par) distanceMap(key) = Math.min(distanceMap(key), distance(next, key))
      seeds += next
      log("Iteration done")
    }
    seeds.toList
  }

  def getKClusters(first: DNASequence, all: Iterable[DNASequence], k: Int) = {
    val seeds = getKSeeds(first, all, k)
    all.groupBy(s => seeds.minBy(g => distance(g, s)))
  }
}
