package edu.gsu.cs.butterfly.exec

import edu.gsu.cs.butterfly.exec.Ukkonen.editCost
/**
 * Seed finder based on edit distance
 * implemented through Ukkonen's method
 */
object UkkonenSeedFinder extends SeedFinder {
  def distance(arg1: String, arg2: String): Int = {
    val dist = editCost(arg1, arg2)
    return dist
  }
}
