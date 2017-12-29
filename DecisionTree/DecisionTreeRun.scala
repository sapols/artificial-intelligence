package test

import main.{CarExample, DecisionTreeBuilder, TreeNode}

import scala.collection.mutable.Map

/**
  * Created by Shawn Polson on 10/16/17.
  *
  * This is the portion of the code which runs all the code.
  */
object DecisionTreeRun extends App {

  val log2 = (x: Double) => {
    if (x==0) 0.0
    else math.log10(x)/math.log10(2.0)
  }


  val dtb = new DecisionTreeBuilder()

  val root = new TreeNode("", List.empty, 25, 25, 8, 17)

  dtb.buildDecisionTree(root)

  root.print

}
