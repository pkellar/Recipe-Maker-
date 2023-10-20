package kellar_patrick

/**
* Program Name: Scala, Composite, RDP, and Chain of Responsibility Patterns
* Author: Patrick Kellar
* Class: CSC-461-M01 Fall 2022
* Description: This is a basic recipe software.
* Last tier passed: 6
*
0. Got it running						__x___
1.	Add + Display*	36
Prompts correct 						__x___
Adds each item 						 	__x___
Above displays correctly formatted 		__x___


2A) Remove + Display*	10
Prompts correct							__x___
Removes and displays correctly 			__x___


2B) Add + XML save*	14
Console added items saved correctly 		__x___
Console added multiples is saved correctly 	__x___


2C) XML load + XML save*	14
1 XML file loaded and saved correctly 	__x___
2+ XML file loaded and saved correctly	__x___


2D) XML load + Display*	12
1 XML file loaded and displays correctly 	_x____
2+ XML file loaded and displays correctly	_x____


2E) XML+ Display with bad file handing	10
All errors handled 							__x___


3.	Stress test for above*	12
Loads in file, adds data, and displays/saves correctly			__x___
Appends a file and displays/saves correctly 					__x___
Removes ingredient after edits, and displays/saves correctly 	__x___


4. Find ingredient*	16
CoR format at least there						__x___
First item found and output formatted correctly	__x___
Handles “not found case”						__x___


5A.	Calculate calories*	7
Correct with no remeasuring		__x___
Correct with remeasuring		__x___
Parallelized* 					___x__

5B.  Calculate volume 7						__x___
Correct with no remeasuring or baking		__x___
Correct with remeasuring and baking 		__x___
Parallelized* 								__x___


6. Calculate density count 6				__x___


Every Line with a * has its grading tag: __x___

**/

import java.io.File
import java.text.DecimalFormat
import scala.io.StdIn
import scala.collection.mutable.ListBuffer
import scala.xml.*
import java.io.FileWriter
import kellar_patrick.XMLHelper.makeNode

object MainStarter {
  /**
   * Description: Will output a menu of choices that let you utilize a recipe book
   *
   * @param args the starting arguments for this program
   */
  def main(args: Array[String]): Unit = {
    //Format used for outputting doubles
    val format = new DecimalFormat("##.#")
    val recipeBook = new RecipeBook()
    val menu: String =
      """
        |1) Add Data
        |2) Display Data
        |3) Remove Recipe
        |4) Load XML
        |5) Write XML
        |6) Find Ingredient in Recipe
        |7) Calculate Calories
        |8) Calculate Volume
        |9) Calculate Calorie Density
        |0) Quit
        |
        |Choice:> """.stripMargin
    var choice: Any = -1

    while (choice != "0") {
      print(menu)
      choice = getInput

      choice match {

        case "1" => //Add Data
          print("What recipe:> ")
          val recipeName = getInput

          //Check if recipe is already in the book before adding it in
          if (!recipeBook.findRecipe(recipeName)) {

            //GRADING: ADD
            recipeBook.addRecipe(Recipe(recipeName))
          }
          else { //If already in book, print message
            println(recipeName + " is already in the book")
          }

        //GRADING: PRINT
        case "2" => print(recipeBook)

        case "3" => //Remove recipe
          print("What recipe:> ")
          val recipeName = getInput.toLowerCase

          //Recipe book returns true if successfully removed
          if (recipeBook.removeRecipe(recipeName)) {
            println("Removed " + recipeName)
          }
          else {
            println("Recipe not found")
          }

        case "4" => //Load XML
          print("file name:> ")
          val fileName = getInput

          //Check if file exists
          if (File(fileName).exists()) {
            val topNode = XML.loadFile(fileName) //XML.loadFile will read in the DOM tree

            //Checking if the top label is correct
            if (topNode.label == "recipebook") {
              //GRADING: READ
              recipeBook.loadXml(topNode)
            } else { //print warning if incorrect top label
              println("invalid xml file. needs to be an recipebook xml file")
            }
          }
          else { //If file doesn't exist, print warning
            println("could not open file: " + fileName + " (the system cannot find the file specified)")
          }

        case "5" => //Write XML
          print("file name:> ")
          val fileName = getInput

          //Calling PrettyPrinter class with 80 columns and 3 space indentation
          val prettyPrinter = new PrettyPrinter(80, 3)

          //GRADING: WRITE
          val newXml = recipeBook.writeXml() //Returns XML to save to file

          //Reformat xml
          val prettyXml = prettyPrinter.format(newXml)

          //Make file, write xml to it, close
          val write = new FileWriter(fileName)
          write.write(prettyXml)
          write.close()

        case "6" => //Find ingredient
          print("recipe:> ")
          val ingredientName = getInput.toLowerCase

          //GRADING: FIND
          if (!recipeBook.findIngredient(ingredientName)) {
            println(ingredientName + " not found")
          }

        case "7" => //Calculate Calories
          print("what recipe:> ")
          val recipeName = getInput.toLowerCase

          val calories = recipeBook.calcCalories(recipeName)
          println("calorie count: " + format.format(calories))

        case "8" => //Calculate Volume
          print("what recipe:> ")
          val recipeName = getInput.toLowerCase

          val volume = recipeBook.calcVolume(recipeName)
          println("volume in cups: " + format.format(volume))

        case "9" => //Calculate Density
          val densityFormat = new DecimalFormat("###.0") //Output format for densities
          val densities = recipeBook.calcDensity() //Will return list of recipe names and their densities
          for (nameDensity <- densities) { //_1 is the name, _2 is the density
            println(nameDensity._1 + ": " + densityFormat.format(nameDensity._2))
          }

        case _ =>
      }
    }
  }

  /**
   * Description: Will get and return user input
   *
   * @return string user entered that isn't a newline
   */
  def getInput: String = {
    var input = StdIn.readLine()
    while (input.isEmpty)
      input = StdIn.readLine()
    input
  }

}

