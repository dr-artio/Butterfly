package edu.gsu.cs.butterfly.model

import java.util.NoSuchElementException
import scala.collection.mutable.HashMap

/**
 * Created by Alexander Artyomenko on 4/22/14.
 */
class ChainHashMap[K, V](cmp: (V, V) => Int) {
  private val hashMap = new HashMap[K, Stream[V]]()
  private val ord = new Ordering[V] {
    override def compare(v1: V, v2: V): Int = {
      cmp(v1, v2)
    }
  }

  def this(map: scala.collection.Seq[(K, V)], compare: (V, V) => Int) = {
    this(compare)
    map foreach (x => put(x._1, x._2))
  }

  def apply(key: K): V = {
    if (!(hashMap.keySet contains key) || hashMap(key).length < 1)
      throw new NoSuchElementException("key: %s not found".format(key))
    hashMap(key).head
  }

  def put(key: K, value: V) = {
    if (!(hashMap.keySet contains key))
      hashMap.put(key, Stream[V](value))
    else {
      val sl = value #:: hashMap(key)
      val osl = sl.sorted(ord)
      hashMap.put(key, osl)
    }
  }

  def getNth(key: K, i: Int): Option[V] = {
    val sl = hashMap(key)
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
