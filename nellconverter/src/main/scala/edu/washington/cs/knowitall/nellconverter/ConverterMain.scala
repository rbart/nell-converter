package edu.washington.cs.knowitall.nellconverter
import java.io.File
import edu.washington.cs.knowitall.tool.chunk.OpenNlpChunker

object ConverterMain extends App {

  val Entity = 0
  val Relation = 1
  val Value = 2
  val Sources = 5
  
  val patternFile = io.Source.fromFile("NELL_patterns.csv").getLines
  val chunker = new OpenNlpChunker
  val relationStrings = io.Source.fromFile("nellrelations_to_humanformat.csv").getLines.map(line => line.split("\t"))
  
  // process the file with relation strings to get a map from a 
  // nell relation to (human-formatted processed string, human-formatted string) 
  import scala.collection.mutable.HashMap
  val relationMap = HashMap[String, Tuple2[String, String]]()
  for (relation <- relationStrings) {
    relationMap += (relation(0).toLowerCase -> (relation(1), relation(2)))
  }
  
  for (line <- io.Source.fromFile("NELL_relations_620.csv").getLines.map(line => line.split("\t"))
      if !isFirstLine(line)) {
    val arg1 = getEntityString(line)
    val rel = getRelationString(line)
    val arg2 = getValueString(line)
    println(arg1 + " " + rel + " " + arg2)
  }
 
  def isFirstLine(line: Array[String]): Boolean = {
    line(Relation) == "Relation"
  }
  
  // gets the "best entity literal string" from a line from the belief file
  def getEntityString(line: Array[String]): String = {
    val length = line.length
    // index of entity string will be: 
    //   length - 2 (if no categories), 
    //   length - 3 (if one category), or 
    //   length - 4 (if two categories) 
    if (!line(length - 1).startsWith("concept:")) line(length - 2)
    else if (!line(length - 2).startsWith("concept:")) line(length - 3)
    else line(length - 3)
  }
  
  // gets the "best value literal string" from a line from the belief file
  def getValueString(line: Array[String]): String = {
    val length = line.length
    // index of value string will be: 
    //   length - 2 (if no categories), 
    //   length - 3 (if one category), or 
    //   length - 4 (if two categories) 
    if (!line(length - 1).startsWith("concept:")) line(length - 1)
    else if (!line(length - 2).startsWith("concept:")) line(length - 2)
    else line(length - 3)
  }
  
  // gets the literal string representation of a relation from a line from the belief file
  def getRelationString(line: Array[String]): String = {
    // get the non-human-formatted relation string sans the "concept:"
    val relation = line(Relation).substring("concept:".length)
    println(relation)
    val relationPair = relationMap.get(relation)
    
    relationPair.getOrElse(throw new IllegalArgumentException())._1
  }
  
}