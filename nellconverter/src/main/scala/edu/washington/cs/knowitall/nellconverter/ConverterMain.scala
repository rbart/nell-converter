package edu.washington.cs.knowitall.nellconverter
import java.io.File
import edu.washington.cs.knowitall.tool.chunk.OpenNlpChunker
import scala.collection.mutable.ListBuffer

object ConverterMain extends App {

  val Entity = 0
  val Relation = 1
  val Value = 2
  val Sources = 5
  
  val patternFile = io.Source.fromFile("NELL_patterns_620.csv").getLines.map(line => line.split("\t"))
  val relationFile = io.Source.fromFile("nellrelations_to_humanformat.csv").getLines.map(line => line.split("\t"))
  val chunker = new OpenNlpChunker
  
  // process the file with relation strings to get a map from a 
  // nell relation to (human-formatted processed string, human-formatted string) 
  import scala.collection.mutable.HashMap
  val relationMap = HashMap[String, Tuple2[String, String]]()
  for (relation <- relationFile) {
    relationMap += (relation(0).toLowerCase -> (relation(1), relation(2)))
  }
  
  val RelationEntity = 0
  val PatternValue = 2
  
  val patternMap = HashMap[String, ListBuffer[String]]()
  for (line <- patternFile) {
    patternMap get line(RelationEntity) match {
      case Some(str) => str += line(PatternValue)
      case None => patternMap += (line(RelationEntity) -> ListBuffer(line(PatternValue)))
    }
  }
  
  for (line <- io.Source.fromFile("NELL_relations_620.csv").getLines.map(line => line.split("\t"))
      if !isFirstLine(line)) {
    val arg1 = getEntityString(line)
    val rel = getRelationString(line)
    val arg2 = getValueString(line)
    println(arg1 + " " + rel + " " + arg2)
  }
  
  /** Determines whether line is the first line in the nell beliefs file.
   *  @param line a line from the nell beliefs file.
   *  @return true if first line; false otherwise.
   */
  def isFirstLine(line: Array[String]): Boolean = {
    line(Relation) == "Relation"
  }
  
  /** Gets the "Best Entity literalString" from a line from the belief file.
   *  @param a line from the nell beliefs file
   *  @return line's "Best Entity literalString" value.  
   */  def getEntityString(line: Array[String]): String = {
    getEVString(line, 'entity)
  }
  
  /** Gets the "Best Value literalString" from a line from the belief file.
   *  @param a line from the nell beliefs file
   *  @return line's "Best Value literalString" value.  
   */
  def getValueString(line: Array[String]): String = {
    getEVString(line, 'value)
  }
  
  // method that gets either the value string or the entity string
  def getEVString(line: Array[String], x: Symbol): String = {
    // offset is how far from the end of the line to go to find the entity/value.
    val offset = if (x == 'entity) 1 else 0
    
    if (hasTwoCategories(line)) line(line.length - 3 - offset)
    else if (hasOneCategory(line)) line(line.length - 2 - offset)
    else line(line.length - 1 - offset)
  }
  
  // gets the literal string representation of a relation from a line from the belief file
  def getRelationString(line: Array[String]): String = {
    // get the non-human-formatted relation string sans the "concept:"
    val relation = line(Relation).substring("concept:".length)
    println(relation)
    val relationPair = relationMap.get(relation)
    
    relationPair.getOrElse(throw new IllegalArgumentException())._1
  }
  
  /** Determines whether a line from the beliefs file has both 
   *  a value for "Categories for Entity" and "Categories for Value"
   *  @param line a line from the nell beliefs file.
   *  @return true if line contains exactly two categories; false otherwise
   */
  def hasTwoCategories(line: Array[String]): Boolean = {
    val length = line.length
    line(length - 1).startsWith("concept:") && line(length - 2).startsWith("concept:")
  } 
  
  /** Determines whether a line from the beliefs file only has 
   *  a value for "Categories for Entity" or "Categories for Value".
   *  @param line a line from the nell beliefs file.
   *  @return true if line contains exactly one category; false otherwise
   */
  def hasOneCategory(line: Array[String]): Boolean = {
    val length = line.length
    line(length - 1).startsWith("concept:") && !line(length - 2).startsWith("concept:")
  }
  
  /** Determines whether a line from the beliefs file has no categories. 
   *  @param line a line from the nell beliefs file.
   *  @return true if line contains no categories; false otherwise. 
   */
  def hasNoCategories(line: Array[String]): Boolean = {
    val length = line.length
    !line(length - 1).startsWith("concept:") && !line(length - 2).startsWith("concept:")
  }
}