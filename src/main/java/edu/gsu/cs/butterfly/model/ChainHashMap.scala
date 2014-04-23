package edu.gsu.cs.butterfly.model

import scala.collection.mutable.{ListBuffer, HashMap}
import java.util.NoSuchElementException

/**
 * Created by Alexander Artyomenko on 4/22/14.
 */
class ChainHashMap[K, V] {
  private val hashMap = new HashMap[K, ListBuffer[V]]()

  def this(map: HashMap[K, V]) = {
    this()
    map foreach (x => put(x._1, x._2))
  }

  def apply(key: K): V = {
    if (!(hashMap.keySet contains key) || hashMap.apply(key).size < 1)
      throw new NoSuchElementException("key: %s not found".format(key))
    hashMap.apply(key).last
  }

  def put(key: K, value: V) = {
    if (!(hashMap.keySet contains key))
      hashMap.put(key, new ListBuffer[V])
    hashMap.apply(key) += value
  }

  def getNth(key: K, i: Int): Option[V] = {
    val lb = hashMap.apply(key)
    if (i >= lb.size)
      return None
    val i_prime = lb.size - i
    Some(lb(i_prime))
  }

  def clipValues(p: (V) => Boolean) = {
    hashMap.keys foreach (k => {
      hashMap.put(k, hashMap.apply(k).filter(p))
    })
  }

  def update(key: K, value: V) = {
    put(key, value)
  }

  def keySet = {
    hashMap.keySet
  }
}
