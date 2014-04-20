package edu.gsu.cs.butterfly.exec

/**
 * Seed finder based on edit distance
 * implemented through Ukkonen's method
 */
object UkkonenSeedFinder extends SeedFinder {
  def distance(arg1: String, arg2: String): Int = {
    val dist = new Ukkonen().editCost(arg1, arg2) - Math.abs(arg1.length - arg2.length)
    return dist
  }
}
