package io.joern.c2cpg.astcreation.converter

import io.joern.c2cpg.astcreation.VAstCreatorNew
import io.joern.c2cpg.astcreation.converter.VAstConverter
import io.joern.x2cpg.Ast
import xtc.tree.Node

abstract class VAstPatternConverter(private val vAstCreator: VAstCreatorNew,
                                    private val converter: VAstConverter,
                                    private val rootNodeTypes: List[String]) {
  
  protected val BLOCK_SPACING: String = "  "
  
  def registerPatternConverter(): (List[String], VAstPatternConverter) = (rootNodeTypes, this)
  
  def convert(superCVAst: Node): Option[Seq[Ast]]

}
