package io.joern.c2cpg.astcreation.converter

import io.joern.c2cpg.astcreation.VAstCreatorNew
import io.joern.x2cpg.Ast
import xtc.tree.Node

class VAstPatternConverterForVariableUse(vAstCreator: VAstCreatorNew, converter: VAstConverter)
  extends VAstPatternConverter(vAstCreator, converter, List.apply("PrimaryIdentifier")) {

  private val variableHandler: VAstVariableHandler = converter.getDeclarationHandler
  
  override def convert(superCVAst: Node, converterState: VAstConverterState): Option[Seq[Ast]] = 
    Option(variableHandler.handleVariableUse(superCVAst, converterState))
}
