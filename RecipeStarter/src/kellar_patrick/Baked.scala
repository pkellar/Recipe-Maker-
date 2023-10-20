package kellar_patrick

/**
 * Author: Patrick Kellar
 * Description: Class that will hold information about a baked ingredient
 **/

import kellar_patrick.MainStarter.getInput
import kellar_patrick.XMLHelper.makeNode
import java.text.DecimalFormat
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.collection.parallel.ParSeq
import scala.xml.*

class Baked(private val level: Int = 1, private var name: String = "", private var expansionFactor: Double = 1.0,
            private val loadNow: Boolean = true)
  extends Ingredient(level: Int, name: String) {
  val TAG = "baked"

  var ingredient: Ingredient = _
  if (loadNow) { //Checks if you want to add a new ingredient manually on initialization
    ingredient = super.newIngredient()
    println("added baked")
  }

  /** Description: Checks if this ingredient matches name and check sub ingredients
   *
   * @param ingredientName - Name of ingredient being searched for
   * @return true if found, false if not
   * */
  override def findIngredient(ingredientName: String): Boolean = {
    if (ingredientName == name) {
      true
    }
    else {
      //Check sub ingredients
      ingredient.findIngredient(ingredientName)
    }
  }

  /** Description: Returns calorie list with ingredient's calories added
   *
   * @param calList - List of calories 
   * @return Updated list of calories
   * */
  override def getCalories(calList: List[Double]): List[Double] = {
    //Pass current calorie list into ingredient and return updated list
    ingredient.getCalories(calList)
  }

  /** Description: Returns volume list with ingredient's name
   *
   * @param volList - List of volumes and their ingredient's name 
   * @return Updated list of volumes
   * */
  override def getVolume(volList: List[(String, Double)] ): List[(String, Double)] = {
    //Get the ingredient's updated volume
    val tempList = ingredient.getVolume(volList)
    var returnList = List[(String, Double)]()

    //Iterate through new list
    for (i <- tempList.indices) {
      //Check if original list contain same value in original list
      if (!volList.contains(tempList(i))) {
        //If value not contained in original, apply an expansion factor to the volume, save in return list
        returnList = returnList :+ (tempList(i)(0), tempList(i)(1) * expansionFactor)
      }
    }

    //append original list with updated return list
    volList ::: returnList
  }

  /** Description: Sets baked information from xml node
   *
   * @param node - Node containing baked ingredient information
   * */
  override def loadXml(node: Node): Unit = {
    //Get expansion factor and name of baked ingredient from node
    expansionFactor = node.attribute("expansion").getOrElse(1.0).toString.toDouble
    name = node.attribute("name").getOrElse("").toString
    
    //Get ingredient from xml
    val ingredientInfo: (Ingredient, Node) = super.newXmlIngredient(node, level)
    //load the ingredients info
    ingredientInfo(0).loadXml(ingredientInfo(1))
    //Set baked class ingredient
    ingredient = ingredientInfo(0)
    
    //Set baked ingredient name if blank
    if (name == "") {
      name = "baked " + ingredient.getName
    }
  }

  /** Description: Return string representing baked ingredient
   *
   * @return String of baked ingredient
   * */
  override def toString: String = {
    val indent = "  " * level //Use ingredient level to make correct indent
    indent + name + " (" + expansionFactor + ")\n" + ingredient
  }

  /** Description: Returns xml node representing this baked ingredient
   *
   * @return Node for this baked ingredient
   * */
  override def writeXml(): Elem = {
    //Make a HashMap with the expansion factor and ingredient name
    val attr: mutable.HashMap[String, String] = mutable.HashMap(("expansion", expansionFactor.toString), ("name", name) )
    val ingredientXml = ingredient.writeXml() //Get child node
    XMLHelper.makeNode(TAG, attr, ingredientXml) // Make/return baked node
  }
}

object Baked {
  
  /** Description: Baked constructor for manually adding data
   *
   * @param level - The level of depth in the tree of the parent node
   * @param name - The name of the baked ingredient to add
   * @return new Baked object with parameters manually set
   * */
  def apply(level: Int, name: String): Baked = {
    print("Expansion Factor:> ")
    val expansionFactor = getInput.toDouble
    new Baked(level + 1, name, expansionFactor)
  }

  /** Description: Baked constructor for setting level, but default for other parameters
   *
   * @param level - The level of depth in the tree of the parent node
   * @return new Baked object with most parameters set to default, no manual load
   * */
  def apply(level: Int): Baked = {
    new Baked(level + 1, loadNow = false)
  }

  /** Description: Baked constructor for setting defaults for parameters
   *
   * @return new Baked object with all parameters set to default, no manual load
   * */
  def apply(): Baked = {
    new Baked(loadNow = false)
  }
}
