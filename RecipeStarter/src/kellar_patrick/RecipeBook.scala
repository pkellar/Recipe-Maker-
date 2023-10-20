package kellar_patrick

/**
 * Author: Patrick Kellar
 * Description: Class that will hold information about the recipe book
 **/

import scala.collection.mutable.ListBuffer
import scala.xml.*
import java.io.FileWriter
import kellar_patrick.XMLHelper.makeNode

class RecipeBook(private var recipeBook: ListBuffer[Recipe] = ListBuffer[Recipe]()) {
  val TAG = "recipebook"

  /** Description: Adds a recipe onto the recipe book
   *
   * @param    recipe - Recipe being added
   */
  def addRecipe(recipe: Recipe): Unit = {
    recipeBook += recipe
  }

  /** Description: Calculates the total calories of a give recipe
   *
   * @param    recipeName - Name of recipe calories are being counted for
   * @return   tempCalories - Double of calories in requested recipe
   * */
  def calcCalories(recipeName: String): Double = {
    var calories = 0.0
    var done = false
    var i = 0

    //Goes through recipe book until it finds a match or it is at the end
    while(i != recipeBook.length && !done) {

      //check if the recipe has a matching name
      if (recipeBook(i).hasName(recipeName)) {
        //if so, count calories and mark as done
        calories = recipeBook(i).calcCalories()
        done = true
      }
      i = i + 1
    }
    calories
  }

  /** Description: Calculates the total calorie density of each recipe
   *
   * @return tempDensities - List[(String,Double)] of each recipe's name and density
   * */
  def calcDensity(): List[(String, Double)] = {
    var densities = List[(String, Double)]()

    for (recipe <- recipeBook) { //Append calculated densities of each recipe
      densities = densities :+ recipe.calcDensity()
    }

    densities
  }

  /** Description: Calculates the total volume of a give recipe in cups
   *
   * @param recipeName - Name of recipe volume in cups are being counted for
   * @return tempVolume - Double of volume in requested recipe
   * */
  def calcVolume(recipeName: String): Double = {
    var volume = 0.0
    var done = false
    var i = 0

    //Goes through recipe book until it finds a match or it is at the end
    while (i != recipeBook.length && !done) {

      //check if the recipe has a matching name
      if (recipeBook(i).hasName(recipeName)) {
        //if so, get volume and mark as done
        volume = recipeBook(i).calcVolume()
        done = true
      }
      i = i + 1
    }
    volume
  }

  /** Description: Calculates the total volume of a give recipe in cups
   *
   * @param name - Name of recipe calories are being counted for
   * @return tempVolume - Double of volume in requested recipe
   * */
  def findIngredient(name: String): Boolean = {
    var found = false
    var i = 0

    //Check if at end of recipes or if found
    while(i != recipeBook.length && !found) {
      //Check if ingredient found in next recipe
      found = recipeBook(i).findIngredient(name)
      i = i + 1
    }
    found //T or F
  }

  /** Description: Checks if provided name matches a recipe
   *
   * @param name - Name of recipe that is being checked
   * @return found - Boolean, true if found, false if not
   * */
  def findRecipe(name: String): Boolean = {
    var found = false
    var i = 0

    //Check if at end of recipes or if found
    while (i != recipeBook.length && !found) {
      //Check if recipe matches given name
      found = recipeBook(i).hasName(name)
      i = i + 1
    }

    found //T or F
  }

  /** Description: Loads in the parent xml node loads its recipe
   *
   * @param node - Node whose children are recipes being added to the system
   * */
  def loadXml(node: Node): Unit = {
    //Gets the children of the parent node
    val children = node.child

    //Iterate through the children
    for (child <- children) {
      //Get the child's label
      val tag = child.label

      //Load the recipe labels
      tag match {
        case "recipe" =>
          //Make a recipe and load its children
          val recipe = Recipe()
          recipe.loadXml(child)
          //Add the recipe to the recipe book
          addRecipe(recipe)
        case _ =>
      }
    }
  }

  /** Description: Removes a recipe from the recipe book
   *
   * @param name - Name of recipe that is being removed
   * @return found - Boolean, true if found, false if not
   * */
  def removeRecipe(name: String): Boolean = {
    val tempBook: ListBuffer[Recipe] = ListBuffer[Recipe]()
    var removed = false

    //Go through recipe book and check for matching recipe name to the one provided
    for (recipe <- recipeBook) {

      if (!recipe.hasName(name)) {
        //If name not matching, add to temp list
        tempBook.addOne(recipe)
      }
      else {
        //If name matches, set return boolean
        removed = true
      }
    }

    //Set recipe book to new list
    recipeBook = tempBook
    removed //T or F
  }

  /** Description: Returns string format of recipe book
   *
   * @return str - String of recipe book
   * */
  override def toString: String = {
    var str = ""
    for (recipe <- recipeBook) {
      str += recipe.toString + "\n"
    }
    str
  }

  /**
   * Make a recipe book node
   *
   * @return the new recipe book node
   */
  def writeXml(): Elem = {
    //make recipe nodes
    val recipeXml = recipeBook.map(recipe => recipe.writeXml())
    XMLHelper.makeNode(TAG, null, recipeXml)
  }
}

object RecipeBook {
  def apply(): RecipeBook = {
    new RecipeBook()
  }
}
