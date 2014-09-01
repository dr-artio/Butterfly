package edu.gsu.cs.butterfly.model

import edu.gsu.cs.kgem.exec._
import edu.gsu.cs.kgem.model.KGEM
import org.biojava3.core.sequence.DNASequence

import scala.collection.mutable.{HashMap, ListBuffer}

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
  protected var used_reads_map: HashMap[DNASequence, Boolean] = null

  def distance(arg1: String, arg2: String): Int
  def distance(seq1: DNASequence, seq2: DNASequence) : Int = {
    distance(seq1.getSequenceAsString, seq2.getSequenceAsString)
  }

  def outErrorRates(first: DNASequence, all: Iterable[DNASequence]) = {
    val vals = all.map(x => distance(first, x))
    val lens = all.map(_.getLength)
    log("Errors #: %d".format(vals.sum / vals.size))
    log("Avg len: %d".format(lens.sum / lens.size))
  }

  protected def initKClusterMap(first: DNASequence, all: Iterable[DNASequence], k: Int) = {
    log("Initial distance map started...")
    var m = all.map(x => (x, distance(first, x)))
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
      updateClusterMap(filtered_all, distanceMap, next)
      log("Iteration %d done".format(i))
      i += 1
    }
  }

  def getKClusters(first: DNASequence, all: Iterable[DNASequence], k: Int) = {
    initKClusterMap(first, all, k)
    cluster_map.keySet.groupBy(s => cluster_map(s)._1)
  }

  def getClusters(seqs: Iterable[DNASequence], k: Int) = {
    expansion(seqs, k)
    cluster_map.keySet.groupBy(s => cluster_map(s)._1)
  }

  protected def expansion(seqs: Iterable[DNASequence], k: Int) = {
    val candidates = new ListBuffer[DNASequence]()
    used_reads_map =  HashMap(seqs.map(x => (x, false)).toMap.toSeq: _*)

    val parseqs = seqs.par
    parseqs.tasksupport = edu.gsu.cs.butterfly.exec.getNumProc

    val kgem = doKgem(seqs, k)
    val threshold = KGEM.threshold

    val first = kgem.head
    candidates += first
    val distanceMap = HashMap(seqs.map(x => (x, distance(x, first))).toMap.toSeq: _*)
    cluster_map = new ChainHashMap(seqs.map(x => (x, (first, distanceMap(x)))).toSeq,
      (v1: (DNASequence, Int), v2: (DNASequence, Int)) => v1._2.compareTo(v2._2))
    var cluster: Iterable[DNASequence] = null
    for (seq <- parseqs) {
      if (!used_reads_map(seq)) {
        cluster = cluster_map.keySet.filter(x => distance(x, seq) < distanceMap(x))
        val size = cluster.size
        if (size >= threshold) {
          val kgem = doKgem(cluster, 1)
          val kg = kgem.head
          if (candidates.exists(_.getSequenceAsString.equals(kg.getSequenceAsString))) synchronized {
            cluster.foreach(x => used_reads_map(x) = true)
            log("Used size: %d".format(size))
            log("Active reads left: %d".format(used_reads_map.values.count(!_)))
          }
          candidates += kg
          updateClusterMap(seqs, distanceMap, kg)
        }
      }
    } //while (cluster.size >= threshold)
    //candidates.toList
  }

  private def doKgem(reads: Iterable[DNASequence], k: Int) = synchronized {
    executeKgem(reads.toList, k, edu.gsu.cs.butterfly.exec.getNumProc).map(x => {
      val dna = new DNASequence(x.toIntegralString)
      dna.setOriginalHeader("h%d_f_%.2f".format(x.ID, x.freq))
      dna
    }).toList
  }

  private def updateClusterMap(seqs: Iterable[DNASequence],
                               distanceMap: HashMap[DNASequence, Int],
                               next: DNASequence) = synchronized {
    seqs foreach (s => {
      val dist = distance(next, s)
      val cur = distanceMap(s)
      cluster_map(s) = (next, dist)
      if (dist < cur) {
        distanceMap(s) = dist
      }
    })
  }

  protected def expandStep(candidates: Iterable[DNASequence]) = {

  }
}
