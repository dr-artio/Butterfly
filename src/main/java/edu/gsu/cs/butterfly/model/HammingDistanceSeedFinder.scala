package edu.gsu.cs.butterfly.model

/**
 * Created by aartsiomenka1 on 4/19/14.
 */
object HammingDistanceSeedFinder extends SeedFinder {
  def distance(arg1: String, arg2: String): Int = {
    if (arg1.length != arg2.length) throw new Exception("Strings must be same length")
    var dist = 0
    val len = arg1.length
    for (i <- 0 until len) {
      if (arg1(i) != 'N' && arg2(i) != 'N' && arg1(i) != '-' && arg2(i) != '-' && arg1(i) != arg2(i))
        dist += 1
    }
    dist
  }
}
