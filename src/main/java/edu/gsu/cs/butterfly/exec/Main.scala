package edu.gsu.cs.butterfly.exec


/**
 * Created with IntelliJ IDEA.
 * User: aartsiomenka1
 * Date: 11/22/13
 * Time: 2:17 PM
 * To change this template use File | Settings | File Templates.
 */
object Main {
   def main(args: Array[String]) = {
     preProcessArgs(args)

     // Step 1: Align to original references and
     // prepare to build new ones.
     runErif

     // Step 2: Build new references with kGEM
     runKgem(processFirstKgemArgs)

     // Step 3: Realign all reads to new references
     // TODO: runErif

     // Step 4: Final run kGEM in clustering mode
     // TODO: runKgem
   }
}
