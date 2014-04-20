package edu.gsu.cs.butterfly.exec

import org.biojava3.core.sequence.DNASequence
import scala.collection.mutable.{HashMap, ListBuffer}
import edu.gsu.cs.kgem.exec.log
import edu.gsu.cs.kgem.io.OutputHandler.{mean, sigma}

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
  protected var cluster_map: HashMap[DNASequence, DNASequence] = null

  def distance(arg1: String, arg2: String): Int
  def distance(seq1: DNASequence, seq2: DNASequence) : Int = {
    distance(seq1.getSequenceAsString, seq2.getSequenceAsString)
  }

  def initKClusterMap(first: DNASequence, all: Iterable[DNASequence], k: Int) = {
    log("Initial distance map started...")
    var m = all.filter(_.getLength > 0.97 * first.getLength).map(x => (x, distance(first, x)))
    log("Filtered reads #: %d".format(m.size))
    val f = m.maxBy(_._2)._1
    log(f.getOriginalHeader)
//    val vals = m.view.map(_._2)
//    val avg = mean[Int](vals)
//    val sgm = sigma[Int](vals)
    m = m.map(x => (x._1, distance(x._1, f)))
    val filtered_all = m.view.map(_._1)
    log("Initial distance map computed. Size: %d. Radius: %d".format(m.size, m.view.map(_._2).max))
    val distanceMap = HashMap(m.toMap.toSeq: _*)
    cluster_map = HashMap(filtered_all.map(x => (x, f)).toSeq: _*)
    var i = 1
    while (i < k) {
      log("Iteration %d ...".format(i))
      val next = distanceMap.maxBy(_._2)._1
      log("MaxBy done.")
      filtered_all foreach (key =>  {
        val dist = distance(next, key)
        val cur = distanceMap(key)
        if (dist < cur) {
          cluster_map(key) = next
          distanceMap(key) = dist
        }
      })
      log("Iteration %d done".format(i))
      i += 1
    }
  }

  def getKClusters(first: DNASequence, all: Iterable[DNASequence], k: Int) = {
    initKClusterMap(first, all, k)
    cluster_map.keySet.groupBy(s => cluster_map(s))
  }
}
