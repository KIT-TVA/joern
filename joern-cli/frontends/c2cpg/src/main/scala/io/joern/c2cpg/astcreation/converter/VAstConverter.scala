package io.joern.c2cpg.astcreation.converter

import io.joern.x2cpg.Ast
import xtc.tree.Node

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class VAstConverter {

  private val patternConverters: mutable.Map[String, ListBuffer[VAstPatternConverter]] = mutable.Map.empty

  def addPattern(pattern: VAstPatternConverter): Unit = {
    val (rootNodeTypes: List[String], converter: VAstPatternConverter) = pattern.registerPatternConverter()
    for (rootNodeType: String <- rootNodeTypes) {

      val converterList: Option[ListBuffer[VAstPatternConverter]] = patternConverters.get(rootNodeType)
      if (converterList.isEmpty) {
        patternConverters.addOne(rootNodeType, ListBuffer.apply(converter))
      } else {
        converterList.get += converter
      }
    }
  }

  def addPatterns(patterns: List[VAstPatternConverter]): Unit = {
    for (pattern: VAstPatternConverter <- patterns) addPattern(pattern)
  }

  def convert(superCVAstNode: Node): Seq[Ast] = {
    val nodeType: String = superCVAstNode.getName
    val converterList: Option[ListBuffer[VAstPatternConverter]] = patternConverters.get(nodeType)
    if (converterList == null || converterList.isEmpty) {
      Seq.empty
    } else {
      val converters = converterList.get

      var asts: Seq[Ast] = Seq.empty
      var continueCommand: Boolean = true
      val converterIndex: Int = 0
      while (continueCommand && converterIndex < converters.size) {
        val joernVAst: Option[Seq[Ast]] = converters(converterIndex).convert(superCVAstNode)
        if (joernVAst.isDefined) {
          asts = joernVAst.get
          continueCommand = false
        }
      }
      asts
    }
  }
}
