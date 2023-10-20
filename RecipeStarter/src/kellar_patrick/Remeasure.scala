package kellar_patrick

/**
 * Author: Patrick Kellar
 * Description: Class that will hold information about a remeasure ingredient
 **/

import kellar_patrick.MainStarter.getInput
import kellar_patrick.XMLHelper.makeNode
import java.text.DecimalFormat
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.collection.parallel.ParSeq
import scala.xml.*

class Remeasure(private val level: Int = 1, private var name: String = "", private var newQuantity: Double = 1.0,
                private val loadNow: Boolean = true)
  extends Ingredient(level: Int, name: String){
  val TAG = "remeasure"

  var ingredient: Ingredient = _
  if (loadNow) {  //Checks if you want to add a new ingredient 
    ingredient = super.newIngredient()
    println("added remeasure")
  }

  /** Description: Checks if this ingredient matches name and check sub ingredients
   *
   * @param ingredientName - Name of ingredient being searched for
   * @return true if found, false if not
   * */
  override def findIngredient(ingredientName: String): Boolean = {
    if(ingredientName == name){
      true
    }
    else{
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
    //Get calorie list with the remeasure ingredient's calories added
    var returnList = ingredient.getCalories(calList)

    //Turn the original calorie list into a set
    val filterSet = calList.toSet

    //Filter the original calories out of the list
    returnList = returnList.filterNot(filterSet)

    //Apply the remeasure new quantity onto the new calories
    returnList = returnList.map(_ * newQuantity)

    //Add new calories to original list and return
    calList ::: returnList
  }

  /** Description: Returns volume list with ingredient's name
   *
   * @param volList - List of volumes and their ingredient's name 
   * @return Updated list of volumes
   * */
  override def getVolume(volList: List[(String, Double)]): List[(String, Double)] = {
    //Get the ingredient's updated volume
    val tempList = ingredient.getVolume(volList)
    var returnList = List[(String, Double)]()

    //Iterate through new list
    for (i <- tempList.indices) {
      //Check if original list contain same value in original list
      if(!volList.contains(tempList(i))) {
        //If value not contained in original, apply an expansion factor to the volume, save in return list
        returnList = returnList :+ (tempList(i)(0), tempList(i)(1) * newQuantity)
      }
    }
    
    //append original list with updated return list
    volList ::: returnList
  }

  /** Description: Sets information from xml node
   *
   * @param node - Node containing remeasure ingredient information
   * */
  override def loadXml(node: Node): Unit = {
    //Get quantity of remeasure ingredient from node
    newQuantity = node.attribute("quantity").getOrElse(1.0).toString.toDouble
    name = node.text.filterNot(_.isWhitespace) //Filter whitespace from remeasure name
    
    //Get ingredient from xml
    val ingredientInfo: (Ingredient, Node) = super.newXmlIngredient(node, level)
    //load the ingredients info
    ingredientInfo(0).loadXml(ingredientInfo(1))
    //Set remeasure class ingredient
    ingredient = ingredientInfo(0)
  }

  /** Description: Return string representing remeasure ingredient
   *
   * @return String of remeasure ingredient
   * */
  override def toString: String = {
    val indent = "  " * level //Use ingredient level to make correct indent
    indent + "remeasure to " + newQuantity + " cups\n" + ingredient
  }

  /** Description: Returns xml node representing this remeasure ingredient
   *
   * @return Node for this remeasure ingredient
   * */
  override def writeXml(): Elem = {
    //Make a HashMap with the quantity
    val attr: mutable.HashMap[String, String] = mutable.HashMap(("quantity", newQuantity.toString))
    val ingredientXml = ingredient.writeXml() //Get child node
    XMLHelper.makeNode(TAG, attr, ingredientXml)  // Make/return remeasure node
  }

}

object Remeasure {

  /** Description: Remeasure constructor for manually adding data
   *
   * @param level - The level of depth in the tree of the parent node
   * @param name  - The name of the remeasure ingredient to add
   * @return new Remeasure object with parameters manually set
   * */
  def apply(level: Int, name: String): Remeasure = {
    print("New Quantity:> ")
    val newQuantity = getInput.toDouble

    new Remeasure(level + 1, name, newQuantity)
  }

  /** Description: Remeasure constructor for setting level, but default for other parameters
   *
   * @param level - The level of depth in the tree of the parent node
   * @return new Remeasure object with most parameters set to default, no manual load
   * */
  def apply(level: Int): Remeasure = {
    new Remeasure(level + 1, loadNow = false)
  }

  /** Description: Remeasure constructor for setting defaults for parameters
   *
   * @return new Remeasure object with all parameters set to default, no manual load
   * */
  def apply(): Remeasure = {
    new Remeasure(loadNow = false)
  }
}
