/** This generator takes Nell's KB dump and outputs the same dump with only
 *  the trinary "relation" lines.
 */

 import scala.io.Source

val Relation = 1
val Probability = 4
val Sources = 5

// Usage: "scala <thisfile> <nelldatafilename>
if (args.length > 0) {

  for (line <- Source.fromFile(args(0)).getLines.map(line => line.split("\t"));
       if (!isACategory(line) && (isFirstLine(line) || !isBinary(line)))) {
    println(line.mkString("\t"))
  }
  
} else Console.err.println("Please enter filename")

def isBinary(line: Array[String]): Boolean = {
  line(Relation) == "generalizations"
}

def isFirstLine(line: Array[String]): Boolean = {
  line(Relation) == "Relation"
}

def isACategory(line: Array[String]): Boolean = {
  line(Probability) == "NaN"
}

def isFromHuman(line: Array[String]): Boolean = {
  line(Sources).contains("SpreadsheetEdits")
}

def isFromSeed(line: Array[String]): Boolean = {
  line(Sources).contains("OntologyModifier")
}

def isFromSEAL(line: Array[String]): Boolean = {
  line(Sources).contains("SEAL")
}

def isFromMBL(line: Array[String]): Boolean = {
  line(Sources).contains("MBL")
}

def isFromCPL(line: Array[String]): Boolean = {
  line(Sources).contains("CPL")
}

def isFromCMC(line: Array[String]): Boolean = {
  line(Sources).contains("CMC")
}

def getConfidence(line: Array[String]): Double = {
  line(Probability).toDouble
}

def dblString(x: Int, y: Int): String = {
  ((x.toDouble / y) * 100).toString.take(5) + "%"
}
