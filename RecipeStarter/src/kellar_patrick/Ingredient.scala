package kellar_patrick

/**
 * Author: Patrick Kellar
 * Description: Class that will hold information about an Ingredient
 **/

import kellar_patrick.MainStarter.getInput
import scala.collection.parallel.ParSeq
import scala.xml.*


abstract class Ingredient(level: Int, name: String) extends XmlReadWrite {

  /** Description: Checks if this ingredient matches name and check sub ingredients
   *
   * @param ingredientName - Name of ingredient being searched for
   * @return true if found, false if not
   * */
  def findIngredient(ingredientName: String): Boolean = {false}

  /** Description: Returns calorie list with ingredient's calories added
   *
   * @param calList - List of calories 
   * @return Updated list of calories
   * */
  def getCalories(calList: List[Double]): List[Double] = {null}

  /** Description: Returns the name of the ingredient
   *
   * @return String name of ingredient
   * */
  def getName: String = {this.name}

  /** Description: Returns volume list with ingredient's name
   *
   * @param volList - List of volumes and their ingredient's name 
   * @return Updated list of volumes
   * */
  def getVolume(volList: List[(String, Double)]): List[(String, Double)] = {null}

  /** Description: Make and return new xml ingredient
   *
   * @return new ingredient selected by the user
   * */
  def newIngredient(): Ingredient = {
    var ingredient: Ingredient = null
    //Get ingredient type
    print("What ingredient (mix, baked, remeasure, single):> ")
    val ingredientType = getInput.toLowerCase

    //Check if remeasure since it doesn't have a name prompt
    if ((ingredientType != "remeasure") && (ingredientType != "r")) {
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
    ingredient
  }

  /** Description: Make new ingredient from xml node and level
   *
   * @param node - Node for new ingredient
   * @param level - level of parent node
   * @return Tuple of ingredient and its node
   * */
  def newXmlIngredient(node: Node, level: Int): (Ingredient, Node) = {
    var ingredient: Ingredient = null
    var returnChild: Node = null

    //Get the nodes children
    val children = node.child
    //iterate through children
    for (child <- children) {
      val tag = child.label
      //match child tag to ingredient type

      tag match {
        
        case "single" =>
          ingredient = Single(level)
          returnChild = child
          
        case "mix" =>
          ingredient = Mix(level)
          returnChild = child
          
        case "baked" =>
          ingredient = Baked(level)
          returnChild = child
          
        case "remeasure" =>
          ingredient = Single(level)
          returnChild = child
        case _ =>
      }
    }
    (ingredient, returnChild)
  }
}
