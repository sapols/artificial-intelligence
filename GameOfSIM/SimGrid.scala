package main

import scala.collection.mutable.Map

/**
  * Created by Shawn Polson on 10/16/17.
  *
  * The grid for the game, modeled as a Map
  * of (point, color) -> connected points.
  */
class SimGrid {
  //Grid points
  val A = "A"
  val B = "B"
  val C = "C"
  val D = "D"
  val E = "E"
  val F = "F"
  val G = "G"
  val H = "H"
  val validPoints = List(A,B,C,D,E,F,G,H)

  //(point, color) -> connected points
  var connections: Map[(String, String), List[String]] = Map.empty

  /*
   * Helper function to reset the connections in this grid.
   */
  def setConnections(newConnections: Map[(String, String), List[String]]) = connections = newConnections

  def getGrid = this

  def getConnections = connections

  /*
   * Checks to see if the connection (point -> connectingPoint) or vice versa
   * has already been drawn, and adds that connection if it hasn't.
   */
  def makeMove(color: String, point: String, connectingPoint: String) = {
    assert(validPoints.contains(point) && validPoints.contains(connectingPoint))

    if (!connectionAlreadyExists(point, connectingPoint)) {
      if (connections.contains((point, color))) {
        val origConnections = connections((point, color))
        val newConnections = origConnections ::: List(connectingPoint)

        connections += ((point, color)) -> newConnections
      }
      else connections += ((point, color)) -> List(connectingPoint)
    }
    else {
      throw new Exception("That move has already been made: " + point + "->" + connectingPoint)
    }
  }

  /*
   * Helper function that handles checking if the connection (point -> connectingPoint)
   * or vice versa has already been drawn.
   */
  def connectionAlreadyExists(point: String, connectingPoint: String, grid: SimGrid = this): Boolean = {
    var exists = false

    for (color: String <- List("red","blue")) {
      if (grid.connections.contains((point, color))) {
        if (grid.connections((point, color)).contains(connectingPoint))
          exists = true
      }
      else if (grid.connections.contains((connectingPoint, color))) {
        if (grid.connections((connectingPoint, color)).contains(point))
          exists = true
      }
    }

    exists
  }

  /*
   * Checks if either player has drawn a triangle on the grid.
   * This is achieved by searching for any series of three points
   * in which the third point contains a connection to the first one.
   */
  def containsTriangle = {
    var p1 = ""
    var p2 = ""
    var p3 = ""
    var c = ""
    var triangleFound = false

    for (color: String <- List("red","blue")) {
      for (firstPoint <- validPoints) {
        if (connections.contains((firstPoint, color))) {
          val connectedPoints: List[String] = connections((firstPoint, color))
          connectedPoints.foreach(secondPoint => {
            if (connections.contains((secondPoint, color))) {
              val connectedPoints2: List[String] = connections((secondPoint, color))
              connectedPoints2.foreach(thirdPoint => {
                if (connections.contains((thirdPoint, color))) {
                  val connectedPoints3: List[String] = connections((thirdPoint, color))
                  if (connectedPoints3.contains(firstPoint)) {
                    p1 = firstPoint
                    p2 = secondPoint
                    p3 = thirdPoint
                    c = color
                    triangleFound = true
                  }
                }
              })
            }
          })
        }
      }
    }

    if (triangleFound) {
      println("Uh oh. The " + c + " player has just lost the game.")
      println("The triangle is on points " + p1 + " -> " + p2 + " -> " + p3 + " -> " + p1 + ".")
    }
    triangleFound
  }

  /*
   * Simply prints the key, value map which represents the grid.
   */
  def printGrid = {
    connections.keys.foreach{ i =>
      print( "Key = " + i )
      println(" Value = " + connections(i) )}
  }
}
