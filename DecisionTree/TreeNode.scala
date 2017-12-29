package main

/**
  * Created by Shawn Polson on 11/7/17.
  */
class TreeNode(n: String, c: List[TreeNode], parentNum: Double, carNum: Double, yes: Double, no: Double) {

  val attrName: String = n
  var children: List[TreeNode] = c
  val numCarsOfParent: Double = parentNum
  val numCars: Double = carNum
  val numYes: Double = yes
  val numNo: Double = no

  def addChild(node: TreeNode) = {
    children = children ::: List(node)
  }

  def isLeafNode: Boolean = numYes == 0.0 || numNo == 0.0

  def print: Unit = {
    print("", true)
  }

  def print(prefix: String, isTail: Boolean): Unit = {
    val lineConnector = if(isTail) "└── " else "├── "
    val nodeString = {
      if (numYes == 0.0 || numNo == 0.0) numYes match {
        case 0.0 => "NO"
        case _ => "YES"
      }
      else {
        "[" + numCars + ": " + numYes.asInstanceOf[Int] + "Y, " + numNo.asInstanceOf[Int] + "N ]"
      }
    } //should check stuff
    println(prefix + lineConnector + "("+attrName+") " + nodeString)

    for (i <- 0 until children.size-1) {
      val lineSpace = if(isTail) "    " else "│   "
      children(i).print(prefix + lineSpace, false);
    }

    if (children.size > 0) {
      val lineSpace = if(isTail) "    " else "│   "
      children(children.size-1).print(prefix + lineSpace, true);
    }
  }
}
