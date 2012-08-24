package edu.washington.cs.knowitall.nellconverter

object NellBelief {
  
  // these indices map to fields in nell beliefs. 
  val Entity = 0
  val Relation = 1
  val Value = 2
  val IterationNo = 3
  val Probability = 4
  val Sources = 5
  val Extractions = 6
}

/** This immutable class defines a NELL belief: something that CMU's NELL 
 *  system has extracted from the web as a belief.
 *  This class specifically deals with the relations rather than categories in
 *  the NELL database. 
 *  
 *  Every belief is defined as a tab-separated string. 
 */
case class NellBelief(line: Array[String]) {

  /** Auxiliary constructor allows the line to be passed directly. */
  def this(line: String) = this(line.split("\t"))
  
  val length = line.length - 1
  
  /** returns an array of the extraction patterns used to get this belief, if any.
   *  @return an array of the extraction patterns used; if none, returns
   *          an empty array.
   */
  def getPatterns: Array[String] = {
    
    // how much to drop right by
    val rightOffset = 
      if (hasTwoCategories) 6 
      else if (hasOneCategory) 5
      else 4
    
    // drop everything but the "Candidate Source" field.
    val candidateSources = line.
    drop(NellBelief.Extractions).
    dropRight(rightOffset).
    mkString("\t")
    
    // get only the CPL source
    val CPLSource = candidateSources.
    substring(1, candidateSources.length - 1).
    split(", ").
    filter(source => source.startsWith("CPL-Iter")) 
    
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
        
    // if contains \" \", assume the CPL-Iter is of the form "pat1" "pat2" "pat3
    if (patternsStr.contains("\" \"")) {
      val ret = patternsStr.split("\" \"")
      val length = ret(ret.length - 1).length
      ret(ret.length - 1) = ret(ret.length - 1).substring(0, length - 1) // to drop off the last "
      ret
    } else {
      patternsStr.split("\t").filter(pattern => pattern.length > "arg1arg2".length)
    }
  }
  
  /** Determines whether this is the first line in the nell beliefs file.
   *  @return true if first line; false otherwise.
   */
  def isFirstLine: Boolean = {
    line(NellBelief.Relation) == "Relation"
  }
  
  /** Determines whether line is a relation. 
   *  @return true if a relation belief; false otherwise. 
   */
  def isRelation: Boolean = {
    line(NellBelief.Relation).startsWith("concept:")
  }
  
  /** Gets the "Best Entity literalString" from this line.
   *  @return line's "Best Entity literalString" value.  
   */  def getEntityLiteralString: String = {
    getEVString('entity)
  }
  
  /** Gets the "Best Value literalString" from this line.
   *  @return line's "Best Value literalString" value.  
   */
  def getValueLiteralString: String = {
    getEVString('value)
  }
  
  // method that gets either the value string or the entity string
  def getEVString(x: Symbol): String = {
    // offset is how far from the end of the line to go to find the entity/value.
    val offset = if (x == 'entity) 1 else 0
    
    val litStr = 
      if (hasTwoCategories) line(length - 2 - offset)
      else if (hasOneCategory) line(length - 1 - offset)
      else line(length - offset)
    
    // format the string: replace _ with spaces, capitalize as though proper noun.
    litStr.replace('_', ' ').toLowerCase.split(" ").filter(word => word.length > 0).map(word => {
      // if word begins with lowercase letter, capitalize. 
      if (word.charAt(0) >= 97 && word.charAt(0) <= 122) 
        (word.charAt(0) - 32).toChar + word.drop(1)
      else word
    }).
    mkString(" ")
  }
    
  /** Determines whether this has both a value for "Categories for Entity" 
   *  and "Categories for Value"
   *  @return true if line contains exactly two categories; false otherwise
   */
  def hasTwoCategories: Boolean = {
    line(length).contains("concept:") && line(length - 1).contains("concept:")
  } 
  
  /** Determines whether this has a value for "Categories for Entity" 
   *  or "Categories for Value".
   *  @return true if line contains exactly one category; false otherwise
   */
  def hasOneCategory: Boolean = {
    line(length).contains("concept:") && !line(length - 1).contains("concept:")
  }
  
  /** Determines whether a line from the beliefs file has no categories. 
   *  @return true if line contains no categories; false otherwise. 
   */
  def hasNoCategories: Boolean = {
    !line(length).contains("concept:") && !line(length - 1).contains("concept:")
  }
  
  /** @return Entity field */ 
  def getEntityField = line(NellBelief.Entity)
  
  /** @return Relation field */
  def getRelationField = line(NellBelief.Relation)
  
  /** @return Value field */
  def getValueField = line(NellBelief.Value)
  
  /** @return Iteration of Promotion field */
  def getIterationNoField = line(NellBelief.IterationNo)
  
  /** @return Probability field */
  def getProbabilityField = line(NellBelief.Probability)
  
  /** @return Source field */
  def getSourcesField = line(NellBelief.Sources)
  
  /** @return Candidate Source field */
  def getCandidateSourcesField = line(NellBelief.Extractions)
  
}