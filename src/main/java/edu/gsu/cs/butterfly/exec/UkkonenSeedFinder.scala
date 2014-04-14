package edu.gsu.cs.butterfly.exec

import edu.gsu.cs.butterfly.exec.Ukkonen.editCost
/**
 * Seed finder based on edit distance
 * implemented through Ukkonen's method
 */
object UkkonenSeedFinder extends SeedFinder {
  def distance(arg1: String, arg2: String): Int = {
    if (cache contains (arg1, arg2)) return cache((arg1,arg2))
    val dist = editCost(arg1, arg2)
    cache.put((arg1, arg2), dist)
    cache.put((arg2, arg1), dist)
    return dist
  }
}
