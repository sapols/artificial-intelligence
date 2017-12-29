package test

import main.NeighborhoodModel

/**
  * Created by Shawn Polson on 12/01/17.
  *
  * This is the portion of the code which runs everything.
  *
  *
  * "Repeat the following action four hundred times, or until there are no more
  * dissatisfied occupants:
  *
  *  a. Find a random dissatisfied occupant.
  *  b. Move that occupant to a randomly chosen empty location (leaving a '0' in
  *     the place where the occupant formerly lived; thus, the city should always
  *     have 27 '1' and 27 '2' occupants in total).
  *
  * Show the sixty-digit number representing the city after every 20 iterations, up to
  * four hundred iterations. Does the 'ring city' move toward a 'totally satisfied'
  * state? What does that state look like?"
  */
object NeighborhoodRun extends App {

  val city = new NeighborhoodModel

  println("Original city:")
  println(city)
  println("Satisfied: " + city.getNumSatisfied)
  println("Dissatisfied: " + city.getNumDissatisfied + "\n")

  var i = 0
  //Repeat the action four hundred times, or until there are no more dissatisfied occupants
  while ((i <= 400) && !(city.getNumDissatisfied == 0)) {
    city.moveRandomDissatisfiedOccupant

    //print the city after every 20 iterations
    if (i % 20 == 0 && (i != 0)) {
      println("City after " + i + " iterations:")
      println(city)
      println("Satisfied: " + city.getNumSatisfied)
      println("Dissatisfied: " + city.getNumDissatisfied + "\n")
    }

    i = i+1
  }

  println("Final city state:")
  println(city)
  println("Satisfied: " + city.getNumSatisfied)
  println("Dissatisfied: " + city.getNumDissatisfied)
  println("Iterations taken: " + i)

}