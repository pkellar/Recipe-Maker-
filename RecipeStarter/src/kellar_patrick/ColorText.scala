package kellar_patrick

/**
  * author: Dr. Lisa Rebenitsch
  * description: a helper class that will take a string of text and return
  * a ANSI tagged version that can handle a few colors, bolding, and underlining.
  */
object ColorText {

    object Color extends Enumeration {
        val RED, BLUE, GREEN, YELLOW, MAGENTA, CYAN, GRAY, BLACK, WHITE = Value
    }

    /**
      * ColorString
      *
      * will add ANSI tags to make the text the color given
      *
      * @param text  the text to color
      * @param color the color tou use
      * @return a ANSI colored string
      */
    def colorString(text: String, color: Color.Value): String = colorString(text, color, false, false)

    /**
      * ColorString
      *
      * will add ANSI tags to make the text the color given, and bolded if desired
      *
      * @param text  the text to color
      * @param color the color tou use
      * @param bold  if true, the text will be bolded
      * @return a ANSI colored string
      */
    def colorString(text: String, color: Color.Value, bold: Boolean): String = colorString(text, color, bold, false)

    /**
      * ColorString
      *
      * will add ANSI tags to make the text the color given, and bolded/underlined if desired
      *
      * @param text       the text to color
      * @param color      the color tou use
      * @param bold       if true, the text will be bolded
      * @param underlined if true, the text will be underlined
      * @return
      */
    def colorString(text: String, color: Color.Value, bold: Boolean, underlined: Boolean): String = {
        var cString = "\u001B[" //make that "start tag"
        //append the ANSI color
        if (color eq Color.RED) cString += "31"
        else if (color eq Color.GREEN) cString += "32"
        else if (color eq Color.YELLOW) cString += "33"
        else if (color eq Color.BLUE) cString += "34"
        else if (color eq Color.MAGENTA) cString += "35"
        else if (color eq Color.CYAN) cString += "36"
        else if (color eq Color.GRAY) cString += "38;2;100;100;100"
        else if (color eq Color.BLACK) cString += "30"
        else if (color eq Color.WHITE) cString += "37"
        else cString += "30"
        //append if it following text should be bold or undrlined
        if (bold) cString += ";1"
        if (underlined) cString += ";4"
        //finish the format starting tag, add teh text, and then the "reset" tag.
        cString += "m" + text + "\u001B[0m"
        cString
    }
}
