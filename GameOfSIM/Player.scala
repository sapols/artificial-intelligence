package main

import main.SimGrid
import java.lang.Exception

import scala.collection.mutable.Map

/**
  * Created by Shawn Polson on 10/16/17.
  *
  * Players can either be "red" or "blue"
  * and maintain the current state of the game grid.
  */
class Player(c: String, g: SimGrid) {
  assert(c.equals("red") || c.equals("blue"))
  val color = c
  var grid = g

  /*
   * Try to draw the specified connection on the grid.
   * Error if that move is invalid.
   */
  def makeMove(point: String, connectingPoint: String, moveGrid: SimGrid = grid): Unit = {
    try {
      moveGrid.makeMove(color, point, connectingPoint)
    }
    catch {
      case e => throw new Exception(e.getMessage + ". Try again.")
    }
  }

  /*
   * Produces either "RED" or "BLUE"
   */
  override def toString = color.toUpperCase

  /*
   * Allows the computer player to make a random move while trying
   * really hard not to make a triangle.
   * This is achieved by testing up to 1000 random moves before finally
   * allowing the computer to accept a move that produces a triangle.
   */
  var numTries = 0
  def makeRandomMove(): Unit = {
    //Acquire a test grid to try out random moves
    val testGrid: SimGrid = getTestGrid
    val tempConnections = testGrid.connections
    var point1: String = ""
    var point2: String = ""

    try {
      point1 = getRandomPoint
      point2 = getRandomPoint
      assert(point1 != point2)

      numTries = numTries + 1
      makeMove(point1, point2, testGrid) //test the move on the test grid first
      if (testGrid.containsTriangle && numTries >= 1000) { //There are only 56 possible moves in the game; 1000 is plenty
        println(this.toString + " move: " + point1 + ", " + point2)
        makeMove(point1, point2) //actually make this losing move
        testGrid.setConnections(tempConnections) //undo last test move
        numTries = 0
      }
      else if (testGrid.containsTriangle && numTries < 1000) { //Try up to 1000 times not make a triangle
        testGrid.setConnections(tempConnections) //undo last test move
        throw new Exception("Bad move. Try again!")
      }
      else { //move didn't make a triangle so go ahead and make it
        println(this.toString + " move: " + point1 + ", " + point2)
        makeMove(point1, point2)
        testGrid.setConnections(tempConnections) //undo last test move
        numTries = 0
      }
    }
    catch { //Take advantage of makeMove's exception throwing for invalid moves and recursively try again
      case e => makeRandomMove
    }
  }

  /*
   * Generates a point at random (A-H).
   */
  def getRandomPoint: String = {
    val r = scala.util.Random
    val num = r.nextInt(8)

    var point = ""
    num match {
      case 0 => point = "A"
      case 1 => point = "B"
      case 2 => point = "C"
      case 3 => point = "D"
      case 4 => point = "E"
      case 5 => point = "F"
      case 6 => point = "G"
      case 7 => point = "H"
    }

    point
  }

  /*
   * Creates a copy of the current game grid for the purpose
   * of trying out random moves.
   */
  def getTestGrid: SimGrid = {
    val testMap: Map[(String, String), List[String]] = Map.empty
    val testConnections: Map[(String, String), List[String]] = testMap ++= grid.connections
    val testGrid = new SimGrid
    testGrid.setConnections(testConnections)

    testGrid
  }
}
