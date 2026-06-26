package io.joern.c2cpg.astcreation.converter

import io.joern.c2cpg.astcreation.VAstCreatorNew
import io.joern.x2cpg.Ast
import xtc.tree.Node

class VASTPatternConverterForEmptyDefinition(vAstCreator: VAstCreatorNew, converter: VAstConverter)
  extends VAstPatternConverter(vAstCreator, converter, List("EmptyDefinition")) {

  override def convert(superCVAst: Node, converterState: VAstConverterState): Option[Seq[Ast]] = Option(Seq.empty[Ast])
}
