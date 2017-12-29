package main

import scala.collection.mutable.Map

/**
  * Created by Shawn Polson on 11/6/17.
  */
class CarExample(n: String, attrs: Map[String, String], buy: Boolean) {
  val name = n
  val attributes = attrs
  val buyTheCar = buy

  def getAttributes: Map[String, String] = attributes

  def wouldBuy: Boolean  = buyTheCar

  def equalsOtherCarExample(ex: CarExample) = {
    name.equals(ex.name)
  }

  //----Assert that attributes are as expected-------------------------------------------------------------------------
  assert(attributes.contains("Price"), "This example has an invalid attribute.")
  assert(attributes.contains("Is it a 'junker'?"), "This example has an invalid attribute.")
  assert(attributes.contains("Body style"), "This example has an invalid attribute.")
  assert(attributes.contains("Can it be resold for thousands in profit?"), "This example has an invalid attribute.")
  assert(attributes.contains("Year"), "This example has an invalid attribute.")
  assert(attributes.contains("Tech package?"), "This example has an invalid attribute.")
  assert(attributes.contains("Expensive insurance?"), "This example has an invalid attribute.")
  assert(attributes.contains("Sunroof?"), "This example has an invalid attribute.")

  assert(attributes("Price").equals("< $3,000") || attributes("Price").equals("$3,000-$10,000") ||
    attributes("Price").equals("> $10,000"))
  assert(attributes("Is it a 'junker'?").equals("Yes") || attributes("Is it a 'junker'?").equals("No"))
  assert(attributes("Body style").equals("Truck/SUV") || attributes("Body style").equals("Sedan") ||
    attributes("Body style").equals("Coupe"))
  assert(attributes("Can it be resold for thousands in profit?").equals("Yes") ||
    attributes("Can it be resold for thousands in profit?").equals("No"))
  assert(attributes("Year").equals("< 2005") || attributes("Year").equals("2005-2015") ||
    attributes("Year").equals("> 2015"))
  assert(attributes("Tech package?").equals("Yes") || attributes("Tech package?").equals("No"))
  assert(attributes("Expensive insurance?").equals("Yes") || attributes("Expensive insurance?").equals("No"))
  assert(attributes("Sunroof?").equals("Yes") || attributes("Sunroof?").equals("No"))
  //-------------------------------------------------------------------------------------------------------------------

  override def toString: String = {
    name //add to this
  }
}

object CarExample {
  /*
   * Return a list of the 25 Car Examples I made up.
   */
  def getAllExamples: List[CarExample] = {

    val attrs1: Map[String, String] = Map(
      "Price" -> "$3,000-$10,000",
      "Is it a 'junker'?" -> "No",
      "Body style" -> "Sedan",
      "Can it be resold for thousands in profit?" -> "No",
      "Year" -> "2005-2015",
      "Tech package?" -> "Yes",
      "Expensive insurance?" -> "Yes",
      "Sunroof?" -> "Yes"
    )
    val ex1 = new CarExample("Acura RL", attrs1, true)

    val attrs2: Map[String, String] = Map(
      "Price" -> "< $3,000",
      "Is it a 'junker'?" -> "Yes",
      "Body style" -> "Coupe",
      "Can it be resold for thousands in profit?" -> "No",
      "Year" -> "< 2005",
      "Tech package?" -> "No",
      "Expensive insurance?" -> "No",
      "Sunroof?" -> "Yes"
    )
    val ex2 = new CarExample("Honda Accord", attrs2, false)

    val attrs3: Map[String, String] = Map(
      "Price" -> "< $3,000",
      "Is it a 'junker'?" -> "No",
      "Body style" -> "Sedan",
      "Can it be resold for thousands in profit?" -> "Yes",
      "Year" -> "> 2015",
      "Tech package?" -> "No",
      "Expensive insurance?" -> "Yes",
      "Sunroof?" -> "Yes"
    )
    val ex3 = new CarExample("Infinity G5", attrs3, true)

    val attrs4: Map[String, String] = Map(
      "Price" -> "$3,000-$10,000",
      "Is it a 'junker'?" -> "No",
      "Body style" -> "Sedan",
      "Can it be resold for thousands in profit?" -> "No",
      "Year" -> "2005-2015",
      "Tech package?" -> "No",
      "Expensive insurance?" -> "No",
      "Sunroof?" -> "No"
    )
    val ex4 = new CarExample("Subaru Outback", attrs4, false)

    val attrs5: Map[String, String] = Map(
      "Price" -> "> $10,000",
      "Is it a 'junker'?" -> "No",
      "Body style" -> "Sedan",
      "Can it be resold for thousands in profit?" -> "No",
      "Year" -> "> 2015",
      "Tech package?" -> "Yes",
      "Expensive insurance?" -> "No",
      "Sunroof?" -> "Yes"
    )
    val ex5 = new CarExample("Tesla Model S", attrs5, false)

    val attrs6: Map[String, String] = Map(
      "Price" -> "$3,000-$10,000",
      "Is it a 'junker'?" -> "No",
      "Body style" -> "Sedan",
      "Can it be resold for thousands in profit?" -> "No",
      "Year" -> "2005-2015",
      "Tech package?" -> "Yes",
      "Expensive insurance?" -> "No",
      "Sunroof?" -> "No"
    )
    val ex6 = new CarExample("Acura TL", attrs6, false)

    val attrs7: Map[String, String] = Map(
      "Price" -> "$3,000-$10,000",
      "Is it a 'junker'?" -> "No",
      "Body style" -> "Coupe",
      "Can it be resold for thousands in profit?" -> "No",
      "Year" -> "2005-2015",
      "Tech package?" -> "Yes",
      "Expensive insurance?" -> "No",
      "Sunroof?" -> "No"
    )
    val ex7 = new CarExample("Pontiac G5", attrs7, false)

    val attrs8: Map[String, String] = Map(
      "Price" -> "$3,000-$10,000",
      "Is it a 'junker'?" -> "No",
      "Body style" -> "Sedan",
      "Can it be resold for thousands in profit?" -> "No",
      "Year" -> "> 2015",
      "Tech package?" -> "Yes",
      "Expensive insurance?" -> "No",
      "Sunroof?" -> "No"
    )
    val ex8 = new CarExample("Audi A6", attrs8, true)

    val attrs9: Map[String, String] = Map(
      "Price" -> "< $3,000",
      "Is it a 'junker'?" -> "No",
      "Body style" -> "Sedan",
      "Can it be resold for thousands in profit?" -> "No",
      "Year" -> "< 2005",
      "Tech package?" -> "No",
      "Expensive insurance?" -> "No",
      "Sunroof?" -> "Yes"
    )
    val ex9 = new CarExample("Toyota Avalon", attrs9, false)

    val attrs10: Map[String, String] = Map(
      "Price" -> "$3,000-$10,000",
      "Is it a 'junker'?" -> "No",
      "Body style" -> "Truck/SUV",
      "Can it be resold for thousands in profit?" -> "No",
      "Year" -> "> 2015",
      "Tech package?" -> "Yes",
      "Expensive insurance?" -> "No",
      "Sunroof?" -> "Yes"
    )
    val ex10 = new CarExample("Honda Passport", attrs10, false)

    val attrs11: Map[String, String] = Map(
      "Price" -> "$3,000-$10,000",
      "Is it a 'junker'?" -> "No",
      "Body style" -> "Truck/SUV",
      "Can it be resold for thousands in profit?" -> "No",
      "Year" -> "2005-2015",
      "Tech package?" -> "Yes",
      "Expensive insurance?" -> "No",
      "Sunroof?" -> "Yes"
    )
    val ex11 = new CarExample("Honda Passport 2", attrs11, false)

    val attrs12: Map[String, String] = Map(
      "Price" -> "< $3,000",
      "Is it a 'junker'?" -> "No",
      "Body style" -> "Truck/SUV",
      "Can it be resold for thousands in profit?" -> "No",
      "Year" -> "2005-2015",
      "Tech package?" -> "No",
      "Expensive insurance?" -> "No",
      "Sunroof?" -> "Yes"
    )
    val ex12 = new CarExample("Dodge Ram", attrs12, false)

    val attrs13: Map[String, String] = Map(
      "Price" -> "$3,000-$10,000",
      "Is it a 'junker'?" -> "No",
      "Body style" -> "Sedan",
      "Can it be resold for thousands in profit?" -> "No",
      "Year" -> "2005-2015",
      "Tech package?" -> "Yes",
      "Expensive insurance?" -> "Yes",
      "Sunroof?" -> "Yes"
    )
    val ex13 = new CarExample("Audi A8", attrs13, true)

    val attrs14: Map[String, String] = Map(
      "Price" -> "$3,000-$10,000",
      "Is it a 'junker'?" -> "No",
      "Body style" -> "Sedan",
      "Can it be resold for thousands in profit?" -> "No",
      "Year" -> "2005-2015",
      "Tech package?" -> "Yes",
      "Expensive insurance?" -> "No",
      "Sunroof?" -> "Yes"
    )
    val ex14 = new CarExample("Audi A4", attrs14, true)

    val attrs15: Map[String, String] = Map(
      "Price" -> "> $10,000",
      "Is it a 'junker'?" -> "No",
      "Body style" -> "Sedan",
      "Can it be resold for thousands in profit?" -> "Yes",
      "Year" -> "2005-2015",
      "Tech package?" -> "Yes",
      "Expensive insurance?" -> "Yes",
      "Sunroof?" -> "Yes"
    )
    val ex15 = new CarExample("Bugatti Veyron", attrs15, false)

    val attrs16: Map[String, String] = Map(
      "Price" -> "< $3,000",
      "Is it a 'junker'?" -> "Yes",
      "Body style" -> "Coupe",
      "Can it be resold for thousands in profit?" -> "Yes",
      "Year" -> "< 2005",
      "Tech package?" -> "Yes",
      "Expensive insurance?" -> "No",
      "Sunroof?" -> "No"
    )
    val ex16 = new CarExample("Mercedes Benz", attrs16, false)

    val attrs17: Map[String, String] = Map(
      "Price" -> "$3,000-$10,000",
      "Is it a 'junker'?" -> "No",
      "Body style" -> "Sedan",
      "Can it be resold for thousands in profit?" -> "No",
      "Year" -> "> 2015",
      "Tech package?" -> "Yes",
      "Expensive insurance?" -> "No",
      "Sunroof?" -> "Yes"
    )
    val ex17 = new CarExample("Hyundai Senata", attrs17, true)

    val attrs18: Map[String, String] = Map(
      "Price" -> "< $3,000",
      "Is it a 'junker'?" -> "Yes",
      "Body style" -> "Truck/SUV",
      "Can it be resold for thousands in profit?" -> "No",
      "Year" -> "2005-2015",
      "Tech package?" -> "No",
      "Expensive insurance?" -> "No",
      "Sunroof?" -> "No"
    )
    val ex18 = new CarExample("Subaru Old", attrs18, false)

    val attrs19: Map[String, String] = Map(
      "Price" -> "$3,000-$10,000",
      "Is it a 'junker'?" -> "No",
      "Body style" -> "Coupe",
      "Can it be resold for thousands in profit?" -> "No",
      "Year" -> "2005-2015",
      "Tech package?" -> "Yes",
      "Expensive insurance?" -> "No",
      "Sunroof?" -> "Yes"
    )
    val ex19 = new CarExample("BMW 1", attrs19, false)

    val attrs20: Map[String, String] = Map(
      "Price" -> "$3,000-$10,000",
      "Is it a 'junker'?" -> "No",
      "Body style" -> "Sedan",
      "Can it be resold for thousands in profit?" -> "No",
      "Year" -> "2005-2015",
      "Tech package?" -> "Yes",
      "Expensive insurance?" -> "No",
      "Sunroof?" -> "Yes"
    )
    val ex20 = new CarExample("BMW 2", attrs20, true)

    val attrs21: Map[String, String] = Map(
      "Price" -> "$3,000-$10,000",
      "Is it a 'junker'?" -> "No",
      "Body style" -> "Sedan",
      "Can it be resold for thousands in profit?" -> "No",
      "Year" -> "2005-2015",
      "Tech package?" -> "Yes",
      "Expensive insurance?" -> "No",
      "Sunroof?" -> "No"
    )
    val ex21 = new CarExample("BMW 3", attrs21, false)

    val attrs22: Map[String, String] = Map(
      "Price" -> "$3,000-$10,000",
      "Is it a 'junker'?" -> "No",
      "Body style" -> "Sedan",
      "Can it be resold for thousands in profit?" -> "No",
      "Year" -> "> 2015",
      "Tech package?" -> "No",
      "Expensive insurance?" -> "No",
      "Sunroof?" -> "Yes"
    )
    val ex22 = new CarExample("Hyundai Senata 2", attrs22, false)

    val attrs23: Map[String, String] = Map(
      "Price" -> "$3,000-$10,000",
      "Is it a 'junker'?" -> "No",
      "Body style" -> "Sedan",
      "Can it be resold for thousands in profit?" -> "No",
      "Year" -> "2005-2015",
      "Tech package?" -> "No",
      "Expensive insurance?" -> "No",
      "Sunroof?" -> "Yes"
    )
    val ex23 = new CarExample("Audi A7", attrs23, false)

    val attrs24: Map[String, String] = Map(
      "Price" -> "$3,000-$10,000",
      "Is it a 'junker'?" -> "No",
      "Body style" -> "Sedan",
      "Can it be resold for thousands in profit?" -> "No",
      "Year" -> "2005-2015",
      "Tech package?" -> "No",
      "Expensive insurance?" -> "Yes",
      "Sunroof?" -> "Yes"
    )
    val ex24 = new CarExample("Acura RL 2", attrs24, false)

    val attrs25: Map[String, String] = Map(
      "Price" -> "< $3,000",
      "Is it a 'junker'?" -> "Yes",
      "Body style" -> "Sedan",
      "Can it be resold for thousands in profit?" -> "No",
      "Year" -> "2005-2015",
      "Tech package?" -> "Yes",
      "Expensive insurance?" -> "No",
      "Sunroof?" -> "No"
    )
    val ex25 = new CarExample("Acura RL 3", attrs25, false)

    List(ex1,ex2,ex3,ex4,ex5,ex6,ex7,ex8,ex9,ex10,ex11,ex12,ex13,
         ex14,ex15,ex16,ex17,ex18,ex19,ex20,ex21,ex22,ex23,ex24,ex25)
  }

  def getAllAttributes: Map[String, List[String]] = {
    var attrMap: Map[String, List[String]] = Map.empty

    getAllExamples.foreach(ex => {
      ex.getAttributes.keys.foreach(key => {

        if (!attrMap.contains(key)) {
          attrMap += key -> List(ex.getAttributes(key))
        }
        else {
          val origOptions: List[String] = attrMap(key)
          if (origOptions.forall(o => !o.equals(ex.getAttributes(key)))) {
            val newOptions = origOptions ::: List(ex.getAttributes(key))
            attrMap(key) = newOptions
          }
        }

      })
    })

    attrMap
  }
}
