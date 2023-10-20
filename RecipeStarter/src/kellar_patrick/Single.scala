package kellar_patrick

import kellar_patrick.MainStarter.getInput
import kellar_patrick.XMLHelper.makeNode
import java.text.DecimalFormat
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.collection.parallel.ParSeq
import scala.xml.*

class Single(private val level: Int = 1, private var name: String = "", private var calories: Double = 100.0,
             private var cups: Double = 1.0)
  extends Ingredient(level: Int, name: String){
  private val TAG = "single"
  private val format = new DecimalFormat("0.##")

  /** Description: Checks if this ingredient matches name
   *
   * @param ingredientName - Name of ingredient being searched for
   * @return true if found, false if not
   * */
  override def findIngredient(ingredientName: String): Boolean = {
    if(name == ingredientName){true}
    else{false}
  }

  /** Description: Returns calorie list with ingredient's calories added
   *
   * @param calList - List of calories 
   * @return Updated list of calories
   * */
  override def getCalories(calList: List[Double]): List[Double] = {calList.appended(calories)}

  override def getName: String = name

  /** Description: Returns volume list with ingredient's name
   *
   * @param volList - List of volumes and their ingredient's name 
   * @return Updated list of volumes
   * */
  override def getVolume(volList: List[(String, Double)]): List[(String, Double)] = {volList.appended((name, cups))}

  /** Description: Sets single information from xml node
   *
   * @param node - Node containing single ingredient information
   * */
  override def loadXml(node: Node): Unit = {
    //Set cups, calories, and name of single ingredient from node
    calories = node.attribute("calories").getOrElse(100).toString.toDouble
    cups = node.attribute("cups").getOrElse(1.0).toString.toDouble
    name = node.text
  }

  /** Description: Return string representing single ingredient
   *
   * @return String of single ingredient
   * */
  override def toString: String = {
    val indent = "  " * level
    var str = indent + "______" + name + "______\n"
    str = str + indent + "cups: " + format.format(cups) + "\n"
    str = str + indent + "calories: " + format.format(calories) + "\n"
    str
  }

  /** Description: Returns xml node representing this single ingredient
   *
   * @return Node for this single ingredient
   * */
  override def writeXml(): Elem = {
    //Make a HashMap with the cups and calories of the ingredient
    val attr: mutable.HashMap[String, String] = mutable.HashMap(("cups", cups.toString), ("calories", calories.toString))
    //Get the ingredient name
    val text = Text(name)
    XMLHelper.makeNode(TAG, attr, text) //Make node of single ingredient
  }
}

object Single {

  /** Description: Single constructor for manually adding data
   *
   * @param level - The level of depth in the tree of the parent node
   * @param name  - The name of the single ingredient to add
   * @return new Single object with parameters manually set
   * */
  def apply(level: Int, name: String): Single = {

    print("Calories:> ")
    val calories = getInput.toDouble

    print("Cups:> ")
    val cups = getInput.toDouble

    println("Added single")
    new Single(level + 1, name, calories, cups)
  }

  /** Description: Single constructor for setting level, but default for other parameters
   *
   * @param level - The level of depth in the tree of the parent node
   * @return new Single object with most parameters set to default, no manual load
   * */
  def apply(level: Int): Single = {
    new Single(level + 1)
  }

  /** Description: Single constructor for setting defaults for parameters
   *
   * @return new Single object with all parameters set to default, no manual load
   * */
  def apply(): Single = {
    new Single()
  }
}
