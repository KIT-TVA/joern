package io.joern.c2cpg.astcreation.converter

import io.joern.c2cpg.astcreation.VAstCreatorNew
import io.joern.x2cpg.Ast
import xtc.tree.Node

class VAstPatternConverterForBinaryOperators(vAstCreator: VAstCreatorNew, converter: VAstConverter)
  extends VAstPatternConverter(vAstCreator, converter, List.apply("Initializer")) {

  override def convert(superCVAst: Node): Option[Seq[Ast]] = {
    // TODO: Need to be implemented.
    Option(Seq(vAstCreator.AstHelper()))
  }
}
