package edu.gsu.cs.butterfly.model

import java.util.NoSuchElementException
import scala.collection.mutable.HashMap

/**
 * Created by Alexander Artyomenko on 4/22/14.
 */
class ChainHashMap[K, V](compare: (V, V) => Int) {
  private val hashMap = new HashMap[K, Stream[V]]()
  private val ord = new Ordering[V] {
    override def compare(v1: V, v2: V): Int = {
      this.compare(v1, v2)
    }
  }

  def this(map: scala.collection.Seq[(K, V)], compare: (V, V) => Int) = {
    this(compare)
    map foreach (x => put(x._1, x._2))
  }

  def apply(key: K): V = {
    if (!(hashMap.keySet contains key) || hashMap(key).size < 1)
      throw new NoSuchElementException("key: %s not found".format(key))
    hashMap(key).head
  }

  def put(key: K, value: V) = {
    if (!(hashMap.keySet contains key))
      hashMap.put(key, Stream[V](value))
    else
      hashMap(key) = (value #:: hashMap(key)).sorted(ord)
  }

  def getNth(key: K, i: Int): Option[V] = {
    val sl = hashMap.apply(key)
    if (i >= sl.size || i < 0)
      return None
    Some(sl(i))
  }

  def clipValues(p: (V) => Boolean) = {
    hashMap.keys foreach (k => {
      hashMap.put(k, hashMap(k).filter(p))
    })
  }

  def update(key: K, value: V) = {
    put(key, value)
  }

  def keySet = {
    hashMap.keySet
  }
}
