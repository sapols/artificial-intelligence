package main

import scala.collection.mutable.Map
import scala.collection.mutable.ListBuffer

/**
  * Created by Shawn Polson on 11/6/17.
  */
class DecisionTreeBuilder(attrs: Map[String, List[String]] = CarExample.getAllAttributes) {
  val allAttributes: Map[String, List[String]] = attrs
  var allExamples: List[CarExample] = CarExample.getAllExamples

  /*
   * Returns log base 2 of the given x.
   */
  val log2 = (x: Double) => {
    if (x==0) 0.0
    else math.log10(x)/math.log10(2.0)
  }

  /*
   * Method which builds a decision tree from the car examples.
   */
  def buildDecisionTree(root: TreeNode, attributes: Map[String, List[String]] = allAttributes,
                        examples: List[CarExample] = allExamples, startEntropy: Double = getStartingEntropy): Unit = {
    //check for base case (empty sets or leaf node)
    if (attributes.isEmpty || root.isLeafNode) {
      //Stop
    }
    else {
      val attr: (String, Double) = getInformationGained(attributes, examples, startEntropy)(0)
      val attrName = attr._1
      val attrEntropy = attr._2
      val attrOptions = attributes(attrName)
      var child: TreeNode = null
      attrOptions.foreach(option => {

        //Build the next child node
        val childName = attrName + "," + option
        val numParentCars = root.numCars
        val carsWithAttributeOption: List[CarExample] = getExamplesWithAttributeValue(attrName, option, attributes, examples)
        val numCars = carsWithAttributeOption.size
        var yesCount = 0.0
        var noCount = 0.0
        carsWithAttributeOption.foreach(car => {
          if (car.wouldBuy) yesCount = yesCount + 1.0
          else noCount = noCount + 1.0
        })

        val numYes = yesCount
        val numNo = noCount
        assert(numYes+numNo == numCars, "That's not how math should work...")
        child = new TreeNode(childName, List.empty, numParentCars, numCars, numYes, numNo) //(Needs) n: String, c: List[TreeNode], parentNum: Double, carNum: Double, yes: Double, no: Double
        root.addChild(child)
        removeAttributeValue(attributes, attrName, option) //Correct...?
        val childExamples = carsWithAttributeOption
        if (childExamples.isEmpty) {
          println("Empty examples!") //no-op
        }
        else {
          val childEntropy = getEntropyFromNode(child)
          var attributesWithoutCurrentOne = attributes
          removeAttribute(attributesWithoutCurrentOne, attrName)
          buildDecisionTree(child, attributesWithoutCurrentOne, childExamples, childEntropy)
          //recurse? With new (not global) set of attrs and examples
        }
      })
    }

  }

  def getEntropyFromNode(node: TreeNode) = {
    val numParentCars: Double = node.numCarsOfParent
    val numNodeCars: Double = node.numCars
    val numYes: Double = node.numYes
    val numNo: Double = node.numNo

    val probYes: Double = numYes/numNodeCars
    val probNo: Double = numNo/numNodeCars

    val entropy: Double = (-probYes * log2(probYes)) + (-probNo * log2(probNo))
    entropy
  }

  /*
   * Removed the given list of examples from (CURRENTLY THE GLOBAL allExamples; that might change).
   */
  def removeExamples(/*examples: List[CarExample], */toRemove: List[CarExample]) = {
    val updatedExamples = allExamples.filter(ex => {
      toRemove.forall(c => !ex.equalsOtherCarExample(c))
    })
    allExamples = updatedExamples
  }

  def removeAttribute(attributes: Map[String, List[String]], attr: String) = {
    if (attributes.contains(attr)) {
      attributes -= attr
    }
  }

  def removeAttributeValue(attributes: Map[String, List[String]], attr: String, attrValue: String) = {
    if (attributes.contains(attr)) {
      if (attributes(attr).contains(attrValue)) {
        val origValues = attributes(attr)
        val updatedValues = origValues.filter(v => !v.equals(attrValue))

        if (updatedValues.isEmpty) {
          removeAttribute(attributes, attr)
        }
        else{
          attributes(attr) = updatedValues
        }
      }
    }
  }

  /*
   * Returns the entropy of the training examples before attributes are considered.
   * With my hardcoded set of 25 examples, this returns: 0.9043814577244939
   */
  def getStartingEntropy: Double = {
    var numYes: Double = 0.0
    var numNo: Double = 0.0
    val numExamples: Double = allExamples.size

    allExamples.foreach(ex => {
      if (ex.wouldBuy) numYes = numYes + 1.0
      else numNo = numNo + 1.0
    })

    val probYes: Double = numYes/numExamples
    val probNo: Double = numNo/numExamples

    val entropy: Double = (-probYes * log2(probYes)) + (-probNo * log2(probNo))
    entropy
  }

  def getEntropyOfBinaryAttribute(attrName: String, attributes: Map[String, List[String]] = allAttributes,
                                  examples: List[CarExample] = allExamples): Double = {
    assert(attributes.contains(attrName), "'" + attrName + "' is an invalid attribute.")
    assert(attributes(attrName).size == 2, "'" + attrName + "' is not a binary attribute.")

    val numTrueAttrExamples: Double = getExamplesWithAttributeValue(attrName, "Yes", attributes, examples).size
    val numFalseAttrExamples: Double = getExamplesWithAttributeValue(attrName, "No", attributes, examples).size
    var numTrueYes: Double = 0.0
    var numTrueNo: Double = 0.0
    var numFalseYes: Double = 0.0
    var numFalseNo: Double = 0.0

    getExamplesWithAttributeValue(attrName, "Yes", attributes, examples).foreach(ex => {
      if (ex.wouldBuy) numTrueYes = numTrueYes + 1.0
      else numTrueNo = numTrueNo + 1.0
    })

    getExamplesWithAttributeValue(attrName, "No", attributes, examples).foreach(ex => {
      if (ex.wouldBuy) numFalseYes = numFalseYes + 1.0
      else numFalseNo = numFalseNo + 1.0
    })

    val probTrueYes = numTrueYes/numTrueAttrExamples
    val probTrueNo = numTrueNo/numTrueAttrExamples

    val probFalseYes = numFalseYes/numFalseAttrExamples
    val probFalseNo = numFalseNo/numFalseAttrExamples

    val trueWeight = numTrueAttrExamples/examples.size
    val falseWeight = numFalseAttrExamples/examples.size

    val eTrue: Double = (-probTrueYes * log2(probTrueYes)) + (-probTrueNo * log2(probTrueNo))
    val eFalse: Double = (-probFalseYes * log2(probFalseYes)) + (-probFalseNo * log2(probFalseNo))

    val entropy: Double = trueWeight*eTrue + falseWeight*eFalse
    entropy
  }

  def getEntropyOfThreeValueAttribute(attrName: String, attributes: Map[String, List[String]] = allAttributes,
                                      examples: List[CarExample] = allExamples): Double = {
    assert(attributes.contains(attrName), "'" + attrName + "' is an invalid attribute.")
    assert(attributes(attrName).size == 3, "'" + attrName + "' is not a three-value attribute.")

    val attrValue1 = attributes(attrName)(0)
    val attrValue2 = attributes(attrName)(1)
    val attrValue3 = attributes(attrName)(2)


    val numAttrExamples1: Double = getExamplesWithAttributeValue(attrName, attrValue1, attributes, examples).size
    val numAttrExamples2: Double = getExamplesWithAttributeValue(attrName, attrValue2, attributes, examples).size
    val numAttrExamples3: Double = getExamplesWithAttributeValue(attrName, attrValue3, attributes, examples).size
    var numYes1: Double = 0.0
    var numNo1: Double = 0.0
    var numYes2: Double = 0.0
    var numNo2: Double = 0.0
    var numYes3: Double = 0.0
    var numNo3: Double = 0.0

    getExamplesWithAttributeValue(attrName, attrValue1, attributes, examples).foreach(ex => {
      if (ex.wouldBuy) numYes1 = numYes1 + 1.0
      else numNo1 = numNo1 + 1.0
    })
    getExamplesWithAttributeValue(attrName, attrValue2, attributes, examples).foreach(ex => {
      if (ex.wouldBuy) numYes2 = numYes2 + 1.0
      else numNo2 = numNo2 + 1.0
    })
    getExamplesWithAttributeValue(attrName, attrValue3, attributes, examples).foreach(ex => {
      if (ex.wouldBuy) numYes3 = numYes3 + 1.0
      else numNo3 = numNo3 + 1.0
    })

    val probYes1 = numYes1/numAttrExamples1
    val probNo1 = numNo1/numAttrExamples1
    val probYes2 = numYes2/numAttrExamples2
    val probNo2 = numNo2/numAttrExamples2
    val probYes3 = numYes3/numAttrExamples3
    val probNo3 = numNo3/numAttrExamples3

    val weight1 = numAttrExamples1/examples.size
    val weight2 = numAttrExamples2/examples.size
    val weight3 = numAttrExamples3/examples.size

    val e1: Double = (-probYes1 * log2(probYes1)) + (-probNo1 * log2(probNo1))
    val e2: Double = (-probYes2 * log2(probYes2)) + (-probNo2 * log2(probNo2))
    val e3: Double = (-probYes3 * log2(probYes3)) + (-probNo3 * log2(probNo3))

    val entropy: Double = weight1*e1 + weight2*e2 + weight3*e3
    entropy
  }


  def getInformationGained(attributes: Map[String, List[String]] = allAttributes,
                           examples: List[CarExample] = allExamples,
                           startEntropy: Double = getStartingEntropy): List[(String, Double)] = {
    var entropyListBuff: ListBuffer[(String, Double)] = ListBuffer.empty

    attributes.keys.foreach(key => {
      attributes(key).size match {
        case 2 => {
          val entry: (String, Double) = (key, getEntropyOfBinaryAttribute(key, attributes, examples))
          entropyListBuff += entry
        }
        case 3 => {
          val entry = (key, getEntropyOfThreeValueAttribute(key, attributes, examples))
          entropyListBuff += entry
        }
      }
    })

    val sortedEntropyList = entropyListBuff.toList.sortBy(x => -(startEntropy-x._2)) //Sort by the information gained
    sortedEntropyList
  }

  //----Helper Functions-----------------------------------------------------------------------------------------------

  /*
   * Returns a list of CarExamples that have the specified value for the specified attribute
   */
  def getExamplesWithAttributeValue(attrName: String, attrValue: String,
                                    attributes: Map[String, List[String]] = allAttributes,
                                    examples: List[CarExample] = allExamples): List[CarExample] = {
    assert(attributes(attrName).contains(attrValue), "'" + attrName + "', '" + attrValue +
                                                         "' is not a valid attribute pair.")
    val matchingExamples = examples.filter(ex => {
      val exValue: String = ex.getAttributes(attrName)
      attrValue.equals(exValue)
    })

    matchingExamples
  }

  def printAllInformationGained = {
    var entropyListBuff: ListBuffer[(String, Double)] = ListBuffer.empty

    allAttributes.keys.foreach(key => {
      allAttributes(key).size match {
        case 2 => {
          val entry: (String, Double) = (key, getEntropyOfBinaryAttribute(key))
          entropyListBuff += entry
        }
        case 3 => {
          val entry = (key, getEntropyOfThreeValueAttribute(key))
          entropyListBuff += entry
        }
      }
    })

    var sortedEntropyList = entropyListBuff.toList.sortBy(x => x._2) //Sort by the entropy value
    sortedEntropyList.foreach(x => println(x))
  }

}
