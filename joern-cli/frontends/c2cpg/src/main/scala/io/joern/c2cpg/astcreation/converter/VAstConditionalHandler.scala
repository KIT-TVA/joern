package io.joern.c2cpg.astcreation.converter

import io.joern.x2cpg.Ast
import xtc.tree.Node

trait VAstConditionalHandler {

  def createConditionalSuperCSubtree(conditionalNode: Node,
                                     firstConditionSubtreeCreator: (Node) => Node,
                                     secondConditionSubtreeCreator: (Node) => Node): Node
  
  def handelConditional(conditionalNode: Node,
                        firstConditionSubtreeCreator: (Node) => Seq[Ast],
                        secondConditionSubtreeCreator: (Node) => Seq[Ast]): Seq[Ast]
  
  def isConditionalNode(node: Node): Boolean
}
