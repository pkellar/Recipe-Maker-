package kellar_patrick

import java.io.{FileInputStream, FileNotFoundException, IOException, PrintStream, PrintWriter}
import java.nio.file.{Files, Paths}
import java.util.Scanner
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.control.*

/**
  * This class will test your output against the given file list. This class will be OVERWRITTEN on our end.
  */
object RunTests {
    val CHECK_CAP = false
    val CHECK_LEFT_SPACING = true
    val CHECK_RIGHT_SPACING = false
    val CHECK_NEW_LINE = false
    val MAX_ERROR_LINES = 5
    val CLEAN_UP = true
    val INPUT_PATTERN = ":>"
    val DEFAULT_COLOR: ColorText.Color.Value = ColorText.Color.BLACK
    val PASSED_COLOR: ColorText.Color.Value = ColorText.Color.GREEN
    val FUDGE_LEFT_SPACE = false


    def main(args: Array[String]): Unit = { //no argument try once to get it from the user
        try {
            val tests = Array("tier0", "tier1", "tier2a", "tier2b", "tier2c", "tier2d", "tier2e", "tier3", "tier4",
                "tier5a", "tier5b", "tier6")
            val createdFiles = Array(false, false, false, true, true, false, false, true, false, false, false, false)

            val loopWithBreak = new Breaks;
            loopWithBreak.breakable {
                for ((base, index) <- tests.zipWithIndex) {
                    val inFile = base + ".txt"
                    val runOutFile = base + "-student.out"
                    val answerFile = base + "-solution.out"
                    val runOutFileParsed = base + "-studentInputAdded.out"
                    val answerFileParsed = base + "-InputAdded.out"

                    System.err.println("\n\n\n\n" + "-----------------------------" + base + "-----------------------------")

                    //redirect input and output
                    val output = new PrintStream(runOutFile)
                    val is = new FileInputStream(base + ".txt")


                    Console.withOut(output) {
                        Console.withIn(is) {
                            MainStarter.main(args)
                        }
                    }
                    //close files
                    output.close()
                    is.close()

                    //postProcess to add input
                    val answer = new Scanner(Paths.get(answerFile))
                    var input = new Scanner(Paths.get(inFile))
                    addInput(answer, input, answerFileParsed)
                    val result = new Scanner(Paths.get(runOutFile))
                    input = new Scanner(Paths.get(inFile))
                    addInput(result, input, runOutFileParsed)


                    //check answers--------------------------------------------------------------------------------
                    val error = checkErrors(answerFileParsed, runOutFileParsed)

                    //output pass, or the file with the problem lines
                    if (!error) {
                        System.err.println(ColorText.colorString("Passed", PASSED_COLOR))

                        //check files
                        if (createdFiles(index))
                            checkCreatedFiles(base)

                    }
                    else {

                        printErrors(answerFileParsed, runOutFileParsed)
                    }

                    if (CLEAN_UP) {
                        Thread.sleep(100)
                        if (createdFiles(index))
                            //Files.deleteIfExists(Paths.get(base + "_Out.xml"))
                        Files.deleteIfExists(Paths.get(runOutFileParsed))
                        Files.deleteIfExists(Paths.get(answerFileParsed))
                        Files.deleteIfExists(Paths.get(runOutFile))
                    }

                    if (error) {
                        loopWithBreak.break();
                    }

                }
            }
        } catch {
            case e: Exception =>
                //Any error, use normal console input
                System.err.println("Error redirecting input. Continue with console input...")
                System.err.println("Error was: " + e.toString)
                e.printStackTrace()
        }

    }

    def checkCreatedFiles(base: String): Unit = {
        val createdRunOutFile = base + "_Out.xml"
        val createdAnswerFile = base + "_Sol.xml"
        val answer = new Scanner(Paths.get(createdRunOutFile))
        val result = new Scanner(Paths.get(createdAnswerFile))

        var mismatch: Boolean = false

        //any lines mismatch?
        while (answer.hasNext() && result.hasNext()) {
            val x = answer.nextLine().toLowerCase();
            val y = result.nextLine().toLowerCase()
            if (!x.equals(y))
                mismatch = true
        }
        if (answer.hasNext() || result.hasNext())
            mismatch = true

        //if there is a mismatch, the created file may be incorrect
        if (mismatch)
            System.err.println(ColorText.colorString(
                "BUT, created files need to be doubled checked", PASSED_COLOR))

    }

    @throws[IOException]
    private def checkErrors(answerFileParsed: String, runOutFileParsed: String) = {
        var error = false
        val answer = new Scanner(Paths.get(answerFileParsed))
        val result = new Scanner(Paths.get(runOutFileParsed))

        //check for a mismatch pass
        while (answer.hasNext && result.hasNext) {
            var ansLine = answer.nextLine
            var resultLine = result.nextLine
            ansLine = cleanInput(answer, ansLine)
            resultLine = cleanInput(result, resultLine)

            var equal = ansLine.equals(resultLine)
            if(FUDGE_LEFT_SPACE) {
                equal = equal || (" "+ansLine).equals(resultLine)
                equal = equal || (" "+resultLine).equals(ansLine)
            }
            if (!equal) {
                error = true
            }
        }


        if (answer.hasNext || result.hasNext)
            error = true
        error
    }

    def cleanInput(in: Scanner, line: String): String = {
        var newLine = line
        if (!CHECK_NEW_LINE)
            while (newLine.trim.isEmpty && in.hasNext)
                newLine = in.nextLine

        if (!CHECK_CAP)
            newLine = newLine.toLowerCase

        if (!CHECK_LEFT_SPACING)
            newLine = newLine.stripLeading

        if (!CHECK_RIGHT_SPACING)
            newLine = newLine.stripTrailing

        newLine
    }

    @throws[IOException]
    private def printErrors(answerFileParsed: String, runOutFileParsed: String): Unit = {
        var err_count = 0
        //check answers
        val answer = new Scanner(Paths.get(answerFileParsed))
        val result = new Scanner(Paths.get(runOutFileParsed))
        while (answer.hasNext && result.hasNext) {

            var ansLine = answer.nextLine
            var resultLine = result.nextLine
            ansLine = cleanInput(answer, ansLine)
            resultLine = cleanInput(result, resultLine)

            if (ansLine == resultLine) {
                if (err_count < MAX_ERROR_LINES) {
                    System.err.println(ColorText.colorString(resultLine, DEFAULT_COLOR))
                }
            }
            else {
                if (err_count < MAX_ERROR_LINES)
                    printError(resultLine, ansLine)
                err_count += 1
            }

        }
        //output remaining answer file
        while (answer.hasNext) {
            val ansLine = answer.nextLine
            if (err_count < MAX_ERROR_LINES)
                printError("", ansLine)
            err_count += 1
        }
        //output remaining student result file
        while (result.hasNext) {
            val resultLine = result.nextLine

            if (err_count < MAX_ERROR_LINES)
                printError(resultLine, "")
            err_count += 1
        }
        if (err_count >= MAX_ERROR_LINES)
            System.err.println(ColorText.colorString("More than " + MAX_ERROR_LINES + " lines of errors", ColorText.Color.CYAN, true, true))
    }

    def printError(resultLine: String, ansLine: String): Unit = {
        System.err.println(">>>>>>>Error>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
        System.err.print("Got.....|")
        System.err.println(resultLine + "|")
        System.err.print("Needed..|")
        System.err.println(ansLine + "|")
    }

    def addInput(redirectOutputFile: Scanner, redirectedInputFile: Scanner, fileOut: String): Unit = {
        val lines: ArrayBuffer[String] = ArrayBuffer()
        while (redirectOutputFile.hasNext) {
            val line = redirectOutputFile.nextLine
            val tokens = line.split(INPUT_PATTERN)
            val newLine = new mutable.StringBuilder(tokens(0))
            for (x <- 1 until tokens.length) {
                var next = ""
                try //some spacing in input to readability, strip it
                    while ( {
                        next.isEmpty
                    }) next = redirectedInputFile.nextLine
                catch {
                    case e: Exception =>
                        //something went wrong, usually this mean too many prompts occured
                        next = "NULL"
                }
                newLine.append(INPUT_PATTERN + " ").append(ColorText.colorString(next, ColorText.Color.BLUE, true))
                newLine.append("\n").append(tokens(x).substring(1))
            }
            //somehow the space at the end was stripped.
            //Still need the input
            val index = line.lastIndexOf(INPUT_PATTERN)
            val ifHasPromptLoc = line.length - INPUT_PATTERN.length
            if (index == ifHasPromptLoc) {
                var next = ""
                try while ( {
                    next.isEmpty
                }) next = redirectedInputFile.nextLine
                catch {
                    case e: Exception =>
                        next = "NULL"
                }
                newLine.append(INPUT_PATTERN + " ").append(ColorText.colorString(next, ColorText.Color.BLUE, true))
                newLine.append("\n")
            }
            lines += newLine.toString
        }
        try {
            val print = new PrintWriter(fileOut)
            for (x <- 0 until lines.size) {
                val t = lines(x)
                print.println(t)
            }
            print.close()
        } catch {
            case e: FileNotFoundException =>
                e.printStackTrace()
        }
    }
}
