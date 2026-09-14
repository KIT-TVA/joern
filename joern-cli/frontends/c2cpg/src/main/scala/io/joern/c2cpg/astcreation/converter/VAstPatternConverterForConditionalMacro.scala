package io.joern.c2cpg.astcreation.converter

import io.joern.c2cpg.astcreation.VAstCreatorNew
import io.joern.x2cpg.Ast
import xtc.tree.Node

class VAstPatternConverterForConditionalMacro(vAstCreator: VAstCreatorNew, converter: VAstConverter)
  extends VAstPatternConverter(vAstCreator, converter, List.apply("Conditional")) {
  
  private val conditionalHandler = converter.getConditionalHandler

  override def convert(superCVAst: Node, converterState: VAstConverterState): Option[Seq[Ast]] = {
    val handler: (Node, VAstConverterState) => Seq[Ast] =
      (node: Node, state: VAstConverterState) => converter.convert(node, state)
    val asts: Seq[Ast] = conditionalHandler.handleConditional(superCVAst, converterState, handler)
    Option(asts)
  }
}
