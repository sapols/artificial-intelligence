package test

import main.SimGrid
import main.Player
import scala.io.StdIn._
import scala.util.control.Breaks._

/**
  * Created by Shawn Polson on 10/16/17.
  *
  * This is the portion of the code which runs the game itself.
  */
object SimGameRun extends App {

  //Declare game variables
  val grid: SimGrid = new SimGrid
  val red = new Player("red", grid)
  val blue = new Player("blue", grid)
  var cpu: Player = null

  var input: String = ""
  var currentPlayer: Player = null
  var startPoint = ""
  var endPoint = ""
  var isHumanTurn = true

  //Starting prompt for the game
  print("Are you playing RED? (Y/N): ")
  input = scala.io.StdIn.readLine()
  input match {
    case "Y" => {
      currentPlayer = red
      cpu = new Player("blue", grid)
    }
    case "N" => {
      currentPlayer = blue
      cpu = new Player("red", grid)
    }
  }


  //Take turns of the game
  breakable {
    while (input != "stop") {
      try {
        if (isHumanTurn)
          takeHumanTurn
        else
          takeCPUTurn
      }
      catch {
        case e => {
          if (grid.containsTriangle) input = "stop"
          break
        }
      }
    }
  }

  //Print the game grid at the end of each game.
  println("\nFinal Game Grid:")
  grid.printGrid

  //----Helper functions---------------------------------------------------------------------------

  /*
   * Asks the player for input in the form of "X, X", then makes a move using the provided points.
   * Player is asked to try again if they attempt an invalid move
   * (logic for that is in Player.makeMove).
   */
  def takeHumanTurn: Unit = {
    print(currentPlayer + " move: ")
    input = scala.io.StdIn.readLine
      if (input.equals("stop")) throw new Exception("Break!")
    startPoint = input.split(",")(0).trim
    endPoint = input.split(",")(1).trim

    try {
      currentPlayer.makeMove(startPoint, endPoint)
      isHumanTurn = false
    }
    catch {
      case e => {
        isHumanTurn = true
        println(e.getMessage)
      }
    }

    if (grid.containsTriangle) throw new Exception("Game over!")
  }

  /*
   * The computer player takes turns basically at random
   * (minus that it tries really hard not to make a triangle).
   */
  def takeCPUTurn: Unit = {
    cpu.makeRandomMove
    isHumanTurn = true

    if (grid.containsTriangle) throw new Exception("Game over!")
  }

}
