package main

import main.HouseOccupant
import util.Random.shuffle
import scala.collection.mutable.ArrayBuffer

/**
  * Created by Shawn Polson on 12/01/17.
  */
class NeighborhoodModel {

  //----Instantiate City and check for correctness---------------------------------------------------------------------
  val numOccupants: Int = 60

  var city: Array[HouseOccupant] = Array.fill(numOccupants){new HouseOccupant(0)}
  populateCity

  assert(27 == getNumOccupantsOfType(1), "City must have 27 families of type '1'.")
  assert(27 == getNumOccupantsOfType(2), "City must have 27 families of type '2'.")
  assert(6  == getNumOccupantsOfType(0), "City must have 6 empty houses.")

  //-------------------------------------------------------------------------------------------------------------------

  /*
   * "To begin with, 27 houses should be “occupied” by a family of type 1; 27 houses should be
   * occupied by a family of type 2; and 6 houses should be empty (“type 0”). The
   * initial positions of the occupants should be chosen at random (i.e., there is no
   * deliberate pattern to the initial positions of occupants in the city)."
   */
  def populateCity = {
    val cityBuffer: ArrayBuffer[HouseOccupant] = ArrayBuffer.fill(6){new HouseOccupant(0)}

    //Put 27 "Type 1" families in the city
    for (i <- 0 until 27) {
      cityBuffer += new HouseOccupant(1)
    }

    //Put 27 "Type 2" families in the city
    for (i <- 27 until 54) {
      cityBuffer += new HouseOccupant(2)
    }

    val randomizedCity = shuffle(cityBuffer.toList)
    city = randomizedCity.toArray
  }

  /*
   * Given the index of an occupant, return a list
   * of the four neighboring occupants within two
   * spaces of it.
   *
   * (Throw an exception if the occupant at the given index
   *  is of type 0.)
   */
  def getFourNeighboringOccupants(index: Int): List[HouseOccupant] = {
    assert(index >= 0 && index <= 59, "Attempted to search outside the city limits.")
    var neighborIndices: List[Int] = List.empty

    if (city(index).getFamilyType == 0)
      throw new Exception("This house is empty so it doesn't have 'neighbors'.")
    else {
      //check for edge cases near the "12 o' clock" position of the city
      index match {
        case 0  => neighborIndices = List(59, 58, 1, 2)
        case 1  => neighborIndices = List(0, 59, 2, 3)
        case 59 => neighborIndices = List(58, 57, 0, 1)
        case 58 => neighborIndices = List(57, 56, 59, 0)
        case _  => neighborIndices = List(index-1, index-2, index+1, index+2) //Not an edge case
      }
    }

    val neighbors = neighborIndices.map(i => city(i))
    neighbors
  }

  /*
   * Given the index of an occupant, return true if
   * it has at least two of its own kind among the four
   * neighbors within two spaces of it.
   *
   * (Throw an exception if the occupant at the given index
   *  is of type 0.)
   */
  def occupantIsSatisfied(index: Int): Boolean = {
    assert(index >= 0 && index <= 59, "Attempted to search outside the city limits.")
    val occupantType = city(index).getFamilyType
    var ownKindCount = 0

    if (occupantType == 0)
      throw new Exception("This house is empty so it can't be 'satisfied'.")
    else {
      val neighbors: List[HouseOccupant] = getFourNeighboringOccupants(index)

      neighbors.foreach(o => if (o.getFamilyType == occupantType) ownKindCount = ownKindCount+1)
    }

    ownKindCount >= 2
  }

  /*
   * Return a count of all the dissatisfied occupants in this city.
   */
  def getNumDissatisfied: Int = {
    var dissatisfiedCount = 0

    for (i <- 0 until numOccupants) {
      try {
        if (!occupantIsSatisfied(i))
          dissatisfiedCount = dissatisfiedCount + 1
      }
      catch {
        case e => //no-op. Don't consider empty houses.
      }
    }

    dissatisfiedCount
  }

  /*
   * Return a count of all the satisfied occupants in this city.
   */
  def getNumSatisfied: Int = {
    var satisfiedCount = 0

    for (i <- 0 until numOccupants) {
      try {
        if (occupantIsSatisfied(i))
          satisfiedCount = satisfiedCount + 1
      }
      catch {
        case e => //no-op. Don't consider empty houses.
      }
    }

    satisfiedCount
  }

  /*
   * Return a list of all the indices at which an empty house resides.
   */
  def getEmptyHouseIndices: List[Int] = {
    var emptyHouses: ArrayBuffer[Int] = ArrayBuffer.empty

    for (i <- 0 until numOccupants) {
      if (city(i).getFamilyType == 0)
        emptyHouses += i
    }

    emptyHouses.toList
  }

  /*
   * Given a family type of 1, 2, or 0, return the number
   * of families in this city with that type.
   */
  def getNumOccupantsOfType(t: Int) = {
    assert((t == 1 || t == 2 || t == 0), "Occupant type must be '1', '2', or '0'.")
    var count = 0

    city.foreach(o => {
      if (o.getFamilyType == t) count = count+1
    })

    count
  }

  /*
   * a. Find a random dissatisfied occupant.
   * b. Move that occupant to a randomly chosen empty location (leaving a “0” in
   *    the place where the occupant formerly lived; thus, the city should always
   *    have 27 “1” and 27 “2” occupants in total).
   */
  def moveRandomDissatisfiedOccupant = {
    val notFound = -9999
    var indexOfDissatisfied: Int = notFound
    val random = scala.util.Random

    while (indexOfDissatisfied == notFound) { //while a random dissatisfied occupant hasn't been found, look for one
      try {
        val i = random.nextInt(numOccupants)
        if (!occupantIsSatisfied(i))
          indexOfDissatisfied = i
      }
      catch {
        case e => //no-op. Don't consider empty houses.
      }
    }

    //A random dissatisfied occupant was found, so move them to a random empty house.
    val emptyHouses: List[Int] = getEmptyHouseIndices
    val randomEmptyIndex = emptyHouses(random.nextInt(emptyHouses.size))

    city(randomEmptyIndex) = city(indexOfDissatisfied)
    city(indexOfDissatisfied) = new HouseOccupant(0)
  }

  /*
   * Return this city formatted as a list of its occupants' types.
   */
  override def toString: String = {
    var str = "[ "
    city.foreach(o => str = str + o.getFamilyType + " ")
    str = str + "]"

    str
  }

}
