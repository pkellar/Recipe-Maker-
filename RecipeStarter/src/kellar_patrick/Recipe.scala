package kellar_patrick

/**
 * Author: Patrick Kellar
 * Description: Class that will hold information about a recipe
 **/

import kellar_patrick.MainStarter.getInput
import scala.xml.*
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import kellar_patrick.XMLHelper.makeNode
import scala.collection.parallel.ParSeq
import scala.collection.parallel.CollectionConverters._

class Recipe(private var name: String = "": String, private var ingredient: Ingredient = null) extends XmlReadWrite {
  val TAG = "recipe"

  /** Description: Calculates the total calories of the recipe
   *
   * @return Double of calories in requested recipe
   * */
  def calcCalories(): Double = {
    //Get list of calories from each sub ingredient, paralyze, then add together
    //GRADING: PARALLEL
    ingredient.getCalories(List[Double]()).par.sum
  }

  /** Description: Returns name of recipe with its calorie density per cup
   *
   * @return tuple of name of the recipe with its calorie density
   * */
  def calcDensity(): (String, Double) = {
    //Calculate calories then divide by calculated volume
    (name, calcCalories() / calcVolume())
  }

  /** Description: Calculates the total volume of the recipe
   *
   * @return Double of volume in requested recipe
   * */
  def calcVolume(): Double = {
    //Get ingredient volume, paralyze, sum the doubles in returned list of tuples
    ingredient.getVolume(List[(String, Double)]()).par.map(_._2).sum
  }

  /** Description: Checks if ingredient is in recipe
   *
   * @param ingredientName - Name of ingredient being searched for
   * @return true if found, false if not
   * */
  def findIngredient(ingredientName: String): Boolean = {
    var found = false

    //Checks if ingredient matches the recipes name
    if (ingredientName != name) {
      //Checks if ingredient is in a sub ingredient
      if (ingredient.findIngredient(ingredientName)) {
        found = true
      }
    }
    else {found = true} //If matching recipe name

    if (found) { //Print success if found
      println(ingredientName + " found in " + name)
    }
    found
  }

  /** Description: Checks if recipe name matches provided one
   *
   * @param nameCheck - Name of ingredient being checked against
   * @return true if same, false if not
   * */
  def hasName(nameCheck: String): Boolean = {
    name == nameCheck
  }

  /** Description: Loads in xml node and adds its information to the recipe
   *
   * @param node - Node of a recipe and all its sub ingredients
   * */
  override def loadXml(node: Node): Unit = {
    //Get name of node and its children
    name = node.attribute("name").getOrElse("").toString
    val children = node.child

    //iterate through children
    for (child <- children) {
      
      val tag = child.label
      //match child tag to ingredient type
      tag match {
        
        case "single" =>
          val tempSingle = Single()
          tempSingle.loadXml(child)
          ingredient = tempSingle

        case "remeasure" =>
          val tempRemeasure = Remeasure()
          tempRemeasure.loadXml(child)
          ingredient = tempRemeasure

        case "baked" =>
          val tempBaked = Baked()
          tempBaked.loadXml(child)
          ingredient = tempBaked

        case "mix" =>
          val tempMix = Mix()
          tempMix.loadXml(child)
          ingredient = tempMix
        case _ =>
      }
    }
  }

  /** Description: Returns string of recipe
   *
   * @return string of recipe
   * */
  override def toString: String = {
    "recipe: " + name + "\n==================================\n" + ingredient + "\n"
  }

  /** Description: Return xml node representing a recipe
   *
   * @return true if same, false if not
   * */
  override def writeXml(): Elem = {
    //make recipe node
    val attr: mutable.HashMap[String, String] = mutable.HashMap(("name", name))
    //make child nodes
    val ingredientXml = ingredient.writeXml()
    XMLHelper.makeNode(TAG, attr, ingredientXml)
  }
}

object Recipe {

  /** Description: Recipe constructor with name provided
   *
   * @param name - Name of recipe being added
   * @return true if same, false if not
   * */
  def apply(name: String): Recipe = {
    val level = 0
    var ingredient: Ingredient = null

    //Get user ingredient type
    print("What ingredient (mix, baked, remeasure, single):> ")
    val ingredientType = getInput.toLowerCase

    //Check if remeasure since it doesn't have a name prompt
    if (ingredientType != "remeasure") {
      //Get ingredient name
      print("Name:> ")
      val ingredientName = getInput

      //Make ingredient
      ingredientType match {
        case "mix" | "m" => ingredient = Mix(level, ingredientName)
        case "baked" | "b" => ingredient = Baked(level, ingredientName)
        case "single" | "s" => ingredient = Single(level, ingredientName)
      }
    }
    else {
      //Make remeasure
      ingredient = Remeasure(level, "")
    }
    
    new Recipe(name, ingredient)
  }

  def apply(): Recipe = {
    new Recipe()
  }
}