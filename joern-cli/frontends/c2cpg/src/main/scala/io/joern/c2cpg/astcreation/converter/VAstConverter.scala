package io.joern.c2cpg.astcreation.converter

import io.joern.c2cpg.astcreation.VAstCreatorNew
import io.joern.x2cpg.Ast
import io.shiftleft.codepropertygraph.generated.nodes.NewBlock
import xtc.tree.Node

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class VAstConverter(private val vAstCreator: VAstCreatorNew) {

  private var conditionalHandler: Option[VAstConditionalHandler] = None
  private var initialConverterState: VAstConverterState = VAstConverterState()
  private val patternConverters: mutable.Map[String, ListBuffer[VAstPatternConverter]] = mutable.Map.empty

  def addPattern(pattern: VAstPatternConverter): Unit = {
    val (rootNodeTypes: List[String], converter: VAstPatternConverter, state) = pattern.registerPatternConverter()
    initialConverterState = initialConverterState.updateState(converter, state)
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

  def addConditionalHandler(conditionalHandler: VAstConditionalHandler): Unit = {
    this.conditionalHandler = Option(conditionalHandler)
  }

  def convert(superCVAstNode: Node, converterState: VAstConverterState): Seq[Ast] = {
    val nodeType: String = superCVAstNode.getName
    val converterList: Option[ListBuffer[VAstPatternConverter]] = patternConverters.get(nodeType)
    if (converterList == null || converterList.isEmpty) {
      Seq.empty
    } else {
      val converters = converterList.get

      var asts: Seq[Ast] = Seq.empty
      var continueCommand: Boolean = true
      var converterIndex: Int = 0
      while (continueCommand && converterIndex < converters.size) {
        val joernVAst: Option[Seq[Ast]] = converters(converterIndex).convert(superCVAstNode, converterState)
        if (joernVAst.isDefined) {
          asts = joernVAst.get
          continueCommand = false
        } else {
          converterIndex += 1
        }
      }

      if (asts.isEmpty) {
        createDummyBlock(superCVAstNode)
      } else {
        asts
      }
    }
  }

  private def createDummyBlock(node: Node): Seq[Ast] = {
    val blockNode: NewBlock = vAstCreator.blockNodeHelper(node, "", "<dummy block for missing implementaions>", None, None)
    val ast: Ast = vAstCreator.blockAstHelper(blockNode, List.empty)
    Seq(ast)
  }

  def getConditionalHandler: VAstConditionalHandler = {
    require(conditionalHandler.isDefined, "No Conditional handler is defined.")
    conditionalHandler.get
  }

  def getInitialConverterState: VAstConverterState = initialConverterState
}
