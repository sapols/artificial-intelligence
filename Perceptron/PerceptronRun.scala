package test

import main.Perceptron
import util.Random._

/**
  * Created by Shawn Polson on 12/5/17.
  *
  * This is the portion of the code which runs everything.
  */
object PerceptronRun extends App {

  val p = new Perceptron

  println("*  Starting A weight: " + p.edgeWeightA)
  println("*  Starting B weight: " + p.edgeWeightB)
  println("*  Starting C weight: " + p.edgeWeightC)
  println("*  Starting D weight: " + p.edgeWeightD)
  println("***========================***\n\n\n")

  var i = 0
  //"Show the edge weight values after every 250 training samples, up to about 8000 samples."
  while ((i < 8000) /*&& perceptronHasLearned(p)*/) {
    val sample = getRandomTrainingSample
    p.trainPerceptron(sample._1, sample._2, sample._3)

    //Show the edge weights after every 250 training samples
    if (i % 250 == 0 && (i != 0)) {
      println("Edge weights after " + i + " training samples:")
      println("     A: " + p.edgeWeightA)
      println("     B: " + p.edgeWeightB)
      println("     C: " + p.edgeWeightC)
      println("     D: " + p.edgeWeightD)
      println("   INPUT:  " + sample._1 + " " + sample._2 + " " + sample._3)
      println("   OUTPUT: " + p.getOutput(sample._1, sample._2, sample._3))
      println("\n----------------------------------------\n")
    }

    i = i+1
  }

  println("\n\n\n")
  println("**=====================================**")
  println("**          TRAINING COMPLETE          **")
  println("**=====================================**")
  println(      "Final edge weight values:\n")
  println("A's weight: " + p.edgeWeightA)
  println("B's weight: " + p.edgeWeightB)
  println("C's weight: " + p.edgeWeightC)
  println("D's weight: " + p.edgeWeightD + "\n")
  println("Perceptron's output: ")
  println("000 -> " + p.getOutput(0,0,0))
  println("010 -> " + p.getOutput(0,1,0))
  println("001 -> " + p.getOutput(0,0,1))
  println("110 -> " + p.getOutput(1,1,0) + "\n")
  println("101 -> " + p.getOutput(1,0,1))
  println("111 -> " + p.getOutput(1,1,1))

  //----Helper functions------------------------------------------------------------------------

  /*
   * Produces a random sampling of the input values A, B, and C (values 0 or 1).
   * For each value, get a random number between -1 and 1; if that value is negative,
   * set the corresponding input value to 0, and if it's positive, 1.
   */
  def getRandomTrainingSample: (Int, Int, Int) = {
    val rnd = util.Random

    val aThresh: Double = (-100.0 + rnd.nextInt(200))/100 //Random value between -1 and 1
    val bThresh: Double = (-100.0 + rnd.nextInt(200))/100 //Random value between -1 and 1
    val cThresh: Double = (-100.0 + rnd.nextInt(200))/100 //Random value between -1 and 1

    val a = if(aThresh < 0) 0 else 1
    val b = if(bThresh < 0) 0 else 1
    val c = if(cThresh < 0) 0 else 1

    (a, b, c)
  }

  /*
   * Checks to see if the perceptron is consistently correct as a result of training.
   * This means that regardless of B, if A & C are 1 then output is 1. Else, 0
   * (within like, 3 decimal points).
   * TODO: fucking do this right.
   */
  def perceptronHasLearned(p: Perceptron): Boolean = {
//    var hasLearned: Boolean = true
//    var magicPlusNumber = 4
//
//    var out: String = p.getOutput(1, 0, 1).toString
//    var decimalIndex = out.indexOf(".")
//    var roundedOut = ""
//    try {
//      roundedOut = out.substring(0, decimalIndex + magicPlusNumber)
//    } catch {
//      case e => magicPlusNumber - magicPlusNumber-1
//    }
//    println("roundedOut: " + roundedOut)
//    if (!(roundedOut.equals("1.000") || roundedOut.equals("0.999") || roundedOut.equals("1.001")))
//      hasLearned = false
//
//    out = p.getOutput(1, 1, 1).toString
//    decimalIndex = out.indexOf(".")
//    try {
//      roundedOut = out.substring(0, decimalIndex + magicPlusNumber)
//    } catch {
//      case e => magicPlusNumber - magicPlusNumber-1
//    }
//    println("roundedOut: " + roundedOut)
//    if (!(roundedOut.equals("1.000") || roundedOut.equals("0.999") || roundedOut.equals("1.001")))
//      hasLearned = false
//
//    out = p.getOutput(0, 1, 1).toString
//    decimalIndex = out.indexOf(".")
//    roundedOut = out.substring(0, decimalIndex+4)
//    println("roundedOut: " + roundedOut)
//    if (!(roundedOut.equals("0.000") || roundedOut.equals("-0.999") || roundedOut.equals("0.001"))) //?
//      hasLearned = false
//
//    out = p.getOutput(1, 0, 0).toString
//    decimalIndex = out.indexOf(".")
//    roundedOut = out.substring(0, decimalIndex+4)
//    if (!(roundedOut.equals("0.000") || roundedOut.equals("-0.999") || roundedOut.equals("0.001"))) //?
//      hasLearned = false
//
//    hasLearned
    ???
  }

}
