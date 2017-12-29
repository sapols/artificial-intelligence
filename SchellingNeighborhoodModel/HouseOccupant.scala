package main

/**
  * Created by Shawn Polson on 12/01/17.
  */
class HouseOccupant(t: Int) {
  assert((t == 1 || t == 2 || t == 0), "Family type must be '1', '2', or '0'.")
  val familyType: Int = t

  def getFamilyType = familyType
}
