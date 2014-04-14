package edu.gsu.cs.butterfly.exec


/**
 * Butterfly tool main class
 * Tool is created for performing
 * k-medoid algorithm based on kGEM
 */
object Main {
   def main(args: Array[String]) = {
     parseArguments(args)
     run
   }
}
