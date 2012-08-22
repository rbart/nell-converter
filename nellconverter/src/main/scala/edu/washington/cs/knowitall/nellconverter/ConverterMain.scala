package edu.washington.cs.knowitall.nellconverter
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer

import edu.washington.cs.knowitall.browser.extraction.ReVerbExtraction
import edu.washington.cs.knowitall.collection.immutable.Interval
import edu.washington.cs.knowitall.tool.chunk.OpenNlpChunker

/** Converts from a file containing ONLY NELL's relation beliefs to serialized
 *  ReVerbExtraction objects. 
 *  
 *  Configuration files needed: 
 *    nellrelations_to_humanformat.csv -- for human-readable rels
 *    
 *  Data files needed:
 *    the data file with only relations, no category beliefs. Supply the name of this file as an argument.  
 */
object ConverterMain extends App {
  
  val BaseUrl = """http://rtw.ml.cmu.edu/rtw/kbbrowser/"""
  
  // to separate between belief and pattern in ReVerbExtraction object
  val PatternSeparator = """ | NELL: """
  
  // relevant to relations file
  val Entity = 0
  val Relation = 1
  val Value = 2
  val Extractions = 6
  
  // relevant to relpatterns file
  val RelationEntity = 0
  val PatternValue = 2
  
  val relationFile = io.Source.fromFile("nellrelations_to_humanformat.csv").getLines.map(line => line.split("\t"))
  val chunker = new OpenNlpChunker
  
  // process the file with relation strings to get a map from a 
  // nell relation to (human-formatted processed string, human-formatted string) 
  val relationMap = HashMap[String, Tuple2[String, String]]()
  for (relation <- relationFile) {
    relationMap += (relation(0).toLowerCase.trim -> (relation(1), relation(2)))
  }
  
  for (line <- io.Source.fromFile(args(0)).getLines.map(line => line.split("\t"))
      if !isFirstLine(line) && isRelation(line)) {
    processLine(line)
  }
  
  /** Given a nell belief, prints out the corresponding ReVerbExtraction objects.
   *  @param line a line from the nell beliefs file.
   */
  def processLine(line: Array[String]) = {
    val arg1 = getEntityString(line)
    val relStr = getRelationString(line)
    val arg2 = getValueString(line)
    val belief = relStr replace("arg1", arg1) replace("arg2", arg2)
    
    val arg1Length = arg1.count(char => char == ' ') + 1
    val relLength = relStr.count(char => char == ' ') - 1
    val arg2Length = arg2.count(char => char == ' ') + 1
    
    var arg1Int = Interval.open(0, 0)
    var relInt = Interval.open(0, 0)
    var arg2Int = Interval.open(0, 0)
    
    if (relStr.indexOf("arg1") > relStr.indexOf("arg2")) {
      // handle the case where arg2 comes before arg1
      arg2Int = Interval.open(0, arg2Length)
      relInt = Interval.open(arg2Length, (arg2Length + relLength))
      arg1Int = Interval.open(arg2Length + relLength, (arg2Length + relLength + arg1Length))
      
    } else {
      arg1Int = Interval.open(0, arg1Length)
      relInt = Interval.open(arg1Length, (arg1Length + relLength))
      arg2Int = Interval.open(arg1Length + relLength, arg1Length + relLength + arg2Length)
    }
    
    val url = BaseUrl + line(Entity).drop("concept:".length)
    
    val exactRel = line(Relation).drop("concept:".length)
    
    val patternList = getPatterns(line)
    
    // if no patterns just make one extraction; else print out one extraction for each pattern
    if (patternList.length == 0) {
      val indSeq = chunker.chunk(belief).toIndexedSeq
      val re = new ReVerbExtraction(indSeq, arg1Int, relInt, arg2Int, url)
      println(ReVerbExtraction.serializeToString(re))
    } else {
      for (pattern <- patternList) {
        val fixedPat = pattern replace("arg1", arg1) replace("arg2", arg2)
        val indSeq = chunker.chunk(belief + PatternSeparator + fixedPat).toIndexedSeq
        val re = new ReVerbExtraction(indSeq, arg1Int, relInt, arg2Int, url)
        println(ReVerbExtraction.serializeToString(re))
      }
    }
  }
  
  /** Given a line from the beliefs file that represents a relation, 
   *  returns an array of the extraction patterns used, if any.
   *  
   *  @param line a line from the nell beliefs file representing a relation
   *  @return an array of the extraction patterns used; if none, returns
   *          an empty array.
   */
  def getPatterns(line: Array[String]): Array[String] = {
    
    // how much to drop right by
    val rightOffset = 
      if (hasTwoCategories(line)) 6 
      else if (hasOneCategory(line)) 5
      else 4
    
    // drop everything but the "Candidate Source" field.
    val candidateSources = line 
    .drop(Extractions)
    .dropRight(rightOffset)
    .mkString("\t")
    
    // get only the CPL source
    val CPLSource = candidateSources
    .substring(1, candidateSources.length - 1)
    .split(", ")
    .filter(source => source.startsWith("CPL-Iter")) 
    
    // if no CPL source, return the empty array
    if (CPLSource.length == 0) return Array[String]()
    
    // figure out the index where the extraction patterns start
    val indArg1 = CPLSource(0).indexOf("arg1")
    val indArg2 = CPLSource(0).indexOf("arg2")
    
    val patternsStr = 
      if (indArg1 < indArg2) 
        CPLSource(0).drop(indArg1) 
      else
        CPLSource(0).drop(indArg2)
        
    // if contains " ", the CPL-Iter is of the form "pat1" "pat2" "pat3
    if (patternsStr.contains("\" \"")) {
      val ret = patternsStr.split("\" \"")
      val length = ret(ret.length - 1).length
      ret(ret.length - 1) = ret(ret.length - 1).substring(0, length - 1) // to drop off the last "
      ret
    } else {
      patternsStr.split("\t").filter(pattern => pattern.length > "arg1arg2".length)
    }
  }
  
  /** Determines whether line is the first line in the nell beliefs file.
   *  @param line a line from the nell beliefs file.
   *  @return true if first line; false otherwise.
   */
  def isFirstLine(line: Array[String]): Boolean = {
    line(Relation) == "Relation"
  }
  
  /** Determines whether line is a relation. 
   *  @param line a line from the nell beliefs file.
   *  @return true if a relation belief; false otherwise. 
   */
  def isRelation(line: Array[String]): Boolean = {
    line(Relation).startsWith("concept:")
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
    
    val litStr = 
      if (hasTwoCategories(line)) line(line.length - 3 - offset)
      else if (hasOneCategory(line)) line(line.length - 2 - offset)
      else line(line.length - 1 - offset)
    
    litStr.replace('_', ' ').toLowerCase.split(" ").filter(word => word.length > 0).map(word => {
      if (word.charAt(0) >= 97 && word.charAt(0) <= 122) 
        (word.charAt(0) - 32).toChar + word.drop(1)
      else word
    })
    .mkString(" ")
  }
  
  // gets the literal string representation of a relation from a line from the belief file
  def getRelationString(line: Array[String]): String = {
    // get the non-human-formatted relation string sans the "concept:"
    val relation = line(Relation).substring("concept:".length).trim
    val relationPair = relationMap.get(relation)
    
    relationPair.getOrElse(throw new IllegalArgumentException("relation: " + relation))._1
  }
  
  /** Determines whether a line from the beliefs file has both 
   *  a value for "Categories for Entity" and "Categories for Value"
   *  @param line a line from the nell beliefs file.
   *  @return true if line contains exactly two categories; false otherwise
   */
  def hasTwoCategories(line: Array[String]): Boolean = {
    val length = line.length
    
    line(length - 1).contains("concept:") && line(length - 2).contains("concept:")
  } 
  
  /** Determines whether a line from the beliefs file only has 
   *  a value for "Categories for Entity" or "Categories for Value".
   *  @param line a line from the nell beliefs file.
   *  @return true if line contains exactly one category; false otherwise
   */
  def hasOneCategory(line: Array[String]): Boolean = {
    val length = line.length
    line(length - 1).contains("concept:") && !line(length - 2).contains("concept:")
  }
  
  /** Determines whether a line from the beliefs file has no categories. 
   *  @param line a line from the nell beliefs file.
   *  @return true if line contains no categories; false otherwise. 
   */
  def hasNoCategories(line: Array[String]): Boolean = {
    val length = line.length
    !line(length - 1).contains("concept:") && !line(length - 2).contains("concept:")
  }
}