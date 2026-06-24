package io.joern.c2cpg.astcreation.converter

import io.joern.x2cpg.Ast
import xtc.tree.Node

trait VAstConditionalHandler {

  def createConditionalSuperCSubtree(conditionalNode: Node, conditionSubtreeCreator: (Node) => Node): Node
  
  def handelConditional(conditionalNode: Node, conditionSubtreeCreator: (Node) => Seq[Ast]): Seq[Ast]
  
  def isConditionalNode(node: Node): Boolean

  def getFirstCondition(node: Node): String

  def getSecondCondition(node: Node): Option[String]
  
  def getFirstConditionalSubtree(node: Node): Node
  
  def getSecondConditionalSubtree(node: Node): Option[Node]

  def getAllConditionalAstSubtrees(node: Node): Seq[Node]
}
