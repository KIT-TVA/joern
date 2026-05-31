package io.joern.c2cpg.astcreation.converter

import io.joern.c2cpg.astcreation.VAstCreatorNew
import io.joern.x2cpg.Ast
import io.shiftleft.codepropertygraph.generated.nodes.NewLocal
import xtc.tree.{Location, Node}

class VAstPatternConverterForVariableDeclaration(vAstCreator: VAstCreatorNew, converter: VAstConverter)
  extends VAstPatternConverter(vAstCreator, converter, List.apply("Declaration")) {

  override def convert(superCVAst: Node): Option[Seq[Ast]] = {
    val declarationInformationNode: Node = superCVAst.getNode(0)
    val VariableNameNode: Node = declarationInformationNode.getNode(1)
    
    val variableType: String = declarationInformationNode.getNode(0).getString(0)
    val variableName: String = VariableNameNode.getNode(0).getString(0)

    val location: Location = VariableNameNode.getNode(0).getLocation
    val line: Option[Int] = if (location == null) None else Option(location.line)
    val column: Option[Int] = if (location == null) None else Option(location.column)
    val code: String = s"${variableType} ${variableName}"
    val declaration: NewLocal = vAstCreator.localNodeHelper(declarationInformationNode.getNode(1),
                                                            variableName, code, variableType,
                                                            line=line, column=column)
    val declarationAst: Ast = vAstCreator.AstHelper(declaration)
    
    val initializationOperation: Node = declarationInformationNode.getNode(4)
    if (initializationOperation.size == 0) {
     // If it is a simple single variable declaration
      Option(Seq(declarationAst))
      
    } else {
      // If it is a single variable declaration with value assignment.
      val initializationAsts: Seq[Ast] = converter.convert(initializationOperation.getNode(0))
      val initializationAst: Ast = initializationAsts.head
      Option(Seq(declarationAst, initializationAst))
    }
  }
}
