package kellar_patrick

/**
 * Author: Patrick Kellar
 * Description: Class that will hold information about a mix ingredient
 **/

import kellar_patrick.MainStarter.getInput
import scala.collection.mutable.ListBuffer
import kellar_patrick.XMLHelper.makeNode
import java.text.DecimalFormat
import scala.collection.mutable
import scala.xml.*
import scala.collection.parallel.ParSeq
import scala.collection.parallel.CollectionConverters._


class Mix(private val level: Int = 1, private var name: String = "", private val loadNow: Boolean = true)
  extends Ingredient(level: Int, name: String) {
  val TAG = "mix"
  var ingredients: ListBuffer[Ingredient] = ListBuffer[Ingredient]()
  
  if (loadNow) { //Checks if you want to add a new ingredient
    ingredients = {
      var continue: String = "y"
      var ingredients: ListBuffer[Ingredient] = ListBuffer[Ingredient]()

      //Add ingredients until the user enters n
      while (continue != "n") {
        //Makes new ingredient and adds it to the list
        ingredients = ingredients :+ super.newIngredient()

        //Ask user if they want another ingredient
        print("Add another ingredient (y/n):> ")
        continue = getInput.toLowerCase
      }
      ingredients //Set ingredient list
    }
    println("added mix")
  }

  /** Description: Checks if this ingredient matches name and check sub ingredients
   *
   * @param ingredientName - Name of ingredient being searched for
   * @return true if found, false if not
   * */
  override def findIngredient(ingredientName: String): Boolean = {
    var found = false
    if (name == ingredientName) {
      //if the name of the mix matches, return true
      found = true
    }
    else {
      //else search through mix's ingredients
      for (ingredient <- ingredients) {
        if(!found) { //If not found, ask next ingredient
         found = ingredient.findIngredient(ingredientName)
        }
      }
    }

    found
  }

  /** Description: Adds its ingredient's calories to list
   *
   * @param calList - List of parent calories
   * @return list with mix's calories added on to original list
   * */
  override def getCalories(calList: List[Double]): List[Double] = {
    //Copy the original list
    var tempList: List[Double] = calList
    //Iterate through ingredients
    for(ingredient <- ingredients) {
      //Grab the returned calorie list from ingredients
      tempList = ingredient.getCalories(tempList)
    }
    tempList
  }

  override def getName: String = name

  /** Description: Adds its ingredient's volumes to list
   *
   * @param volList - List of parent volumes
   * @return list with mix's volumes added on to original list
   * */
  override def getVolume(volList: List[(String, Double)] ): List[(String, Double)] = {
    //Copy the original list
    var tempList: List[(String, Double)] = volList
    //Iterate through ingredients
    for (ingredient <- ingredients) {
      //Grab the returned volume list from ingredients
      tempList = ingredient.getVolume(tempList)
    }
    tempList
  }

  /** Description: Sets mix information from xml node
   *
   * @param node - Node containing mix ingredient information
   * */
  override def loadXml(node: Node): Unit = {
    //Get name of mix ingredient from node
    name = node.attribute("name").getOrElse("").toString

    //Get the nodes children
    val children = node.child
    //Iterate through the children
    for (child <- children) {
      //Get their tag and make ingredient type matching
      val tag = child.label
      tag match {
        case "single" =>
          val tempIngredient = Single(level)
          tempIngredient.loadXml(child)
          ingredients += tempIngredient

        case "remeasure" =>
          val tempIngredient = Remeasure(level)
          tempIngredient.loadXml(child)
          ingredients += tempIngredient
        case _ =>
      }
    }

  }

  /** Description: Return string representing mix ingredient
   *
   * @return String of mix ingredient
   * */
  override def toString: String = {
    val indent = "  " * level //Use ingredient level to make correct indent
    var str = indent + name + "\n" + indent + "*****************************\n"

    //Add on string of each ingredient
    for (ingredient <- ingredients) {
      str = str + ingredient
    }
    str + indent + "*****************************\n"
  }

  /** Description: Returns xml node representing this mix ingredient
   *
   * @return Node for this mix ingredient
   * */
  override def writeXml(): Elem = {
    //Make a HashMap with the ingredient name
    val attr: mutable.HashMap[String, String] = mutable.HashMap(("name", name))

    //Get the xml of the mix children ingredients
    val ingredientsXml = ingredients.map(ingredient => ingredient.writeXml())
    XMLHelper.makeNode(TAG, attr, ingredientsXml) //Make Mix Node
  }

}

object Mix {

  /** Description: Mix constructor for manually adding data
   *
   * @param level - The level of depth in the tree of the parent node
   * @param name  - The name of the mix ingredient to add
   * @return new Mix object with parameters manually set
   * */
  def apply(level: Int, name: String): Mix = {
    new Mix(level + 1, name)
  }

  /** Description: Mix constructor for setting level, but default for other parameters
   *
   * @param level - The level of depth in the tree of the parent node
   * @return new Mix object with most parameters set to default, no manual load
   * */
  def apply(level: Int): Mix = {
    new Mix(level + 1, loadNow = false)
  }

  /** Description: Mix constructor for setting defaults for parameters
   *
   * @return new Mix object with all parameters set to default, no manual load
   * */
  def apply(): Mix = {
    new Mix(loadNow = false)
  }
}