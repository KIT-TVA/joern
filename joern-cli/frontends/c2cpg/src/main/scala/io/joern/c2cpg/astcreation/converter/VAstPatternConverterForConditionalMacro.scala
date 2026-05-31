package io.joern.c2cpg.astcreation.converter

import io.joern.c2cpg.astcreation.VAstCreatorNew
import io.joern.x2cpg.{Ast, AstNodeBuilder}
import io.shiftleft.codepropertygraph.generated.ControlStructureTypes
import xtc.tree.Node

class VAstPatternConverterForConditionalMacro(vAstCreator: VAstCreatorNew, converter: VAstConverter)
  extends VAstPatternConverter(vAstCreator, converter, List.apply("Conditional")) {

  private val SIMPLE_CONDITIONAL_MACRO: Int = 2
  
  override def convert(superCVAst: Node): Option[Seq[Ast]] = {
      // TODO: Ignores all conditional statements.
    Some(converter.convert(superCVAst.getNode(1)))
  }

  def getCodeForIfElse(condition: String, ifCode: String, elseCode: String): String = {
    s"#ifdef ${condition}\n${ifCode}\n#else\n${elseCode}\n#endif"
  }

  def getCodeForIf(condition: String, code: String): String = {
    s"#ifdef ${condition}\n${code}\n#endif"
  }
}
