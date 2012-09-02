package edu.washington.cs.knowitall.nellconverter
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import edu.washington.cs.knowitall.browser.extraction.ReVerbExtraction
import edu.washington.cs.knowitall.collection.immutable.Interval
import edu.washington.cs.knowitall.tool.chunk.OpenNlpChunker
import org.slf4j.LoggerFactory;
import org.slf4j.Logger;
import scala.io.Source
import java.io.File

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
  
  val logger = LoggerFactory.getLogger(ConverterMain.getClass.toString);
  
  // to separate between belief and pattern in ReVerbExtraction object
  val PatternSeparator = """ | NELL: """
  
  // used to translate from strings to Seq[ChunkedToken]
  val chunker = new OpenNlpChunker
  
  // used to construct URI for ReVerbExtractions
  val BaseUrl = """http://rtw.ml.cmu.edu/rtw/kbbrowser/"""
  
  logger.info("Parsing command line args.")
  var bels: Option[File] = None
  var rels: Option[File] = None
  
  val parser = new scopt.immutable.OptionParser[Config]("nell-converter", "1.0") {
    def options = Seq(
      arg("<beliefs-file>", "path to file containing NELL beliefs") {
        (path: String, c: Config) => c.copy(beliefs = Some(new File(path)))
      },
      opt("r", "rels", "<relations-file>", "path to file containing mapping from NELL relations to human-readable strings") { 
        (path: String, c: Config) => c.copy(relFile = Some(new File(path))) 
      }
    )
  }
  parser.parse(args, Config()) map { config => 
    bels = config.beliefs
    rels = config.relFile
  }
  
  logger.info("Reading config file with relations, constructing map.")
  val relationMap = HashMap[String, Tuple2[String, String]]()
  var source:scala.io.BufferedSource = null
  try {
    if (rels == None) 
      source = Source.fromInputStream(this.getClass.
                 getResource("nellrelations_to_humanformat.csv").openStream())
    else 
      source = Source.fromFile(rels.get, "UTF-8")
    val relations = source.getLines.
                      map(line => line.split("\t"))
    
    // process the file with relation strings to get a map from a 
    // nell relation to (human-formatted processed string, human-formatted string) 
    for (relation <- relations) {
      relationMap += (relation(0).toLowerCase.trim -> (relation(1), relation(2)))
    }
  } finally {
    if (source != null) {
      source.close()
      logger.info("Finished mapping relations.")
    }
  }
  
  logger.info("Reading data file (beliefs), constructing iterator.")
  try {
    val beliefsFile = bels.getOrElse(throw new IllegalArgumentException("no beliefs file supplied."))
    source = Source.fromFile(beliefsFile, "UTF-8")
    val beliefs = source.getLines.map(line => new NellBelief(line))
    
    logger.info("Processing beliefs, constructing ReVerb objects.")
    // iterate over the beliefs file to construct ReVerb objects
    for (line <- beliefs
         if !line.isFirstLine && line.isRelation) {
      try { processLine(line) }
      catch { case e: Exception => e.printStackTrace }
    }
  } finally {
    if (source != null) {
      source.close()
      logger.info("Finished processing beliefs/converting file.")
    }
  }
  
  /** Given a nell belief, prints out the corresponding ReVerbExtraction objects.
   *  @param line a line from the nell beliefs file.
   */
  def processLine(line: NellBelief) = {
    val arg1 = line.getEntityLiteralString
    val relStr = getRelationLiteralString(line)
    val arg2 = line.getValueLiteralString
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
    
    val url = BaseUrl + line.getEntityField.drop("concept:".length)
    
    val exactRel = line.getRelationField.drop("concept:".length)
    
    val patternList = line.getPatterns
    
    // if no patterns just make one extraction; else print out one extraction for each pattern
    if (patternList.length == 0) {
      val indSeq = chunker.chunk(belief).toIndexedSeq
      val re = new ReVerbExtraction(indSeq, arg1Int, relInt, arg2Int, url)
      println(re.toString)
    } else {
      for (pattern <- patternList) {
        val fixedPat = pattern replace("arg1", arg1) replace("arg2", arg2)
        val indSeq = chunker.chunk(belief + PatternSeparator + fixedPat).toIndexedSeq
        val re = new ReVerbExtraction(indSeq, arg1Int, relInt, arg2Int, url)
        println(re.toString)
      }
    }
  }
  
  // gets the literal string representation of a relation from a line from the belief file
  def getRelationLiteralString(line: NellBelief): String = {
    // get the non-human-formatted relation string sans the "concept:"
    val relation = line.getRelationField.substring("concept:".length).trim

      val relationPair = relationMap.get(relation)
      relationPair.getOrElse({
        logger.error("getRelationLiteralString(): Unknown relation: " + relation)
        throw new IllegalArgumentException()
      })._1
  }
  
  // defines how command line args are parsed.
  case class Config(
    beliefs: Option[File] = None, 
    relFile: Option[File] = None
  )
}