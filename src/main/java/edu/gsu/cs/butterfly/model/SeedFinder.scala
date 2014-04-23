package edu.gsu.cs.butterfly.model

import scala.collection.mutable.HashMap
import org.biojava3.core.sequence.DNASequence
import edu.gsu.cs.kgem.exec._

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
  protected var cluster_map: ChainHashMap[DNASequence, (DNASequence, Int)] = null

  def distance(arg1: String, arg2: String): Int
  def distance(seq1: DNASequence, seq2: DNASequence) : Int = {
    distance(seq1.getSequenceAsString, seq2.getSequenceAsString)
  }

  def initKClusterMap(first: DNASequence, all: Iterable[DNASequence], k: Int) = {
    log("Initial distance map started...")
    var m = all.filter(_.getLength > 0.97 * first.getLength).map(x => (x, distance(first, x)))
    log("Filtered reads #: %d out of %d".format(m.size, all.size))
    val f = m.maxBy(_._2)._1
    log(f.getOriginalHeader)
//    val vals = m.view.map(_._2)
//    val avg = mean[Int](vals)
//    val sgm = sigma[Int](vals)
    m = m.map(x => (x._1, distance(x._1, f)))
    val filtered_all = m.view.map(_._1)
    log("Initial distance map computed. Size: %d. Radius: %d".format(m.size, m.view.map(_._2).max))
    val distanceMap = HashMap(m.toMap.toSeq: _*)
    cluster_map = new ChainHashMap(filtered_all.map(x => (x, (f, distanceMap(x)))).toSeq,
      (v1: (DNASequence, Int), v2: (DNASequence, Int)) => v1._2.compareTo(v2._2))
    var i = 1
    while (i < k) {
      log("Iteration %d ...".format(i))
      val next = distanceMap.maxBy(_._2)._1
      log("MaxBy done.")
      filtered_all foreach (key =>  {
        val dist = distance(next, key)
        val cur = distanceMap(key)
        cluster_map(key) = (next, dist)
        if (dist < cur) {
          distanceMap(key) = dist
        }
      })
      log("Iteration %d done".format(i))
      i += 1
    }
  }

  def getKClusters(first: DNASequence, all: Iterable[DNASequence], k: Int) = {
    initKClusterMap(first, all, k)
    cluster_map.keySet.groupBy(s => cluster_map(s)._1)
  }
}
