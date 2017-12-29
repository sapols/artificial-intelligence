package main

import util.Random._

/**
  * Created by Shawn Polson on 12/5/17.
  */
class Perceptron {

  val rnd = util.Random

  var edgeWeightA: Double = (-100.0 + rnd.nextInt(200))/100 //Random value between -1 and 1
  var edgeWeightB: Double = (-100.0 + rnd.nextInt(200))/100 //Random value between -1 and 1
  var edgeWeightC: Double = (-100.0 + rnd.nextInt(200))/100 //Random value between -1 and 1

  val edgeWeightD: Double = -1
  val inputD: Double      = 1

  val alpha: Double = 0.5 //May have to experiment with several "alpha" values

  /*
  * The main function which trains this perceptron given inputs
  * for A, B, and C. It calls helper functions as needed.
  */
  def trainPerceptron(inputA: Int, inputB: Int, inputC: Int): Unit = {
    assert(inputA == 0 || inputA == 1, "Inputs can only be 0 or 1.")
    assert(inputB == 0 || inputB == 1, "Inputs can only be 0 or 1.")
    assert(inputC == 0 || inputC == 1, "Inputs can only be 0 or 1.")

    val output = getOutput(inputA, inputB, inputC)
    val expected = getExpectedOutput(inputA, inputB, inputC)

    val error = expected - output

    if (error == 0.0) {
      //no-op. If the output is correct, no updating takes place.
    } else {
      //The output is in error, so update the edge weights to shrink the error value.
      updateWeights(inputA, inputB, inputC)
    }
  }

  //----Training helper functions--------------------------------------------------------------------

  /*
   * The perceptron should output a 1 if the input values of A and C
   * are both 1, and 0 otherwise.
   */
  def getExpectedOutput(inputA: Int, inputB: Int, inputC: Int): Double = {
    assert(inputA == 0 || inputA == 1, "Inputs can only be 0 or 1.")
    assert(inputB == 0 || inputB == 1, "Inputs can only be 0 or 1.")
    assert(inputC == 0 || inputC == 1, "Inputs can only be 0 or 1.")

    (inputA, inputC) match {
      case (1, 1) => 1.0
      case _      => 0.0
    }
  }

  /*
   * Given inputs A, B, and C (values 0 or 1),
   * compute this perceptron's output using a
   * continuous sigmoidal function.
   */
  def getOutput(inputA: Int, inputB: Int, inputC: Int): Double = {
    assert(inputA == 0 || inputA == 1, "Inputs can only be 0 or 1.")
    assert(inputB == 0 || inputB == 1, "Inputs can only be 0 or 1.")
    assert(inputC == 0 || inputC == 1, "Inputs can only be 0 or 1.")

    val x = inputA*edgeWeightA + inputB*edgeWeightB + inputC*edgeWeightC + inputD*edgeWeightD
    val output: Double = 1/(1 + math.exp(-x)) //math.exp returns "Euler's number e raised to the power of a double value"

    output
  }

  /*
   * "getOutput" represents the result of this perceptron's sigmoidal function,
   * and this represents the derivative of that function given the same inputs.
   */
  def getDerivativeOfOutputFunc(inputA: Int, inputB: Int, inputC: Int): Double = {
    val outX = getOutput(inputA, inputB, inputC)
    val derivative = outX*(1-outX)

    derivative
  }

  /*
   * Given three inputs A, B, and C (values 0 or 1),
   * update edge weights according to the formula
   * W_j <---- W_j + (a Err g'(in) x_j)
   */
  def updateWeights(inputA: Int, inputB: Int, inputC: Int): Unit = {
    assert(inputA == 0 || inputA == 1, "Inputs can only be 0 or 1.")
    assert(inputB == 0 || inputB == 1, "Inputs can only be 0 or 1.")
    assert(inputC == 0 || inputC == 1, "Inputs can only be 0 or 1.")

    val output = getOutput(inputA, inputB, inputC)
    val expected = getExpectedOutput(inputA, inputB, inputC)

    val err = expected - output
    val gp = getDerivativeOfOutputFunc(inputA, inputB, inputC)

    //Update A
    edgeWeightA = edgeWeightA + (alpha*err*gp*inputA)

    //Update B
    edgeWeightB = edgeWeightB + (alpha*err*gp*inputB)

    //Update C
    edgeWeightC = edgeWeightC + (alpha*err*gp*inputC)
  }

}
