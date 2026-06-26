package io.joern.c2cpg.astcreation.converter

import io.joern.c2cpg.astcreation.VAstCreatorNew
import io.joern.c2cpg.astcreation.converter.{VAstConverter, VAstPatternConverter}
import io.joern.x2cpg.Ast
import io.shiftleft.codepropertygraph.generated.nodes.{NewBlock, NewMethod, NewMethodReturn}
import xtc.tree.Node

class VAstPatternConverterForFunctionDeclaration(vAstCreator: VAstCreatorNew, converter: VAstConverter)
  extends VAstPatternConverter(vAstCreator, converter, List.apply("FunctionDefinition")) {

  private val FUNCTION_DECLARATION: Int = 0
  private val FUNCTION_CODE_INDEX: Int = 1
  
  private val FUNCTION_DEFINITION: String = "FunctionDefinition"
  private val FUNCTION_RETURN_TYPE_ROOT_NODE_NAME: String = "FunctionPrototype"
  private val FUNCTION_NAME_ROOT_NODE_NAME: String = "FunctionDeclarator"
  private val FUNCTION_PARAMETER_ROOT_NODE_NAME: String = "PostfixingFunctionDeclarator"
  
  override def convert(superCVAst: Node, converterState: VAstConverterState): Option[Seq[Ast]] = {

    var methodInstructions: Seq[Ast] = Seq()
    val methodeRootNode: Node = superCVAst.getNode(1).getNode(1)
    val numberOfChildNodes: Int = methodeRootNode.size
    for (nodeIndex: Int <- 0 until numberOfChildNodes) {
      methodInstructions = methodInstructions ++ converter.convert(methodeRootNode.getNode(nodeIndex), converterState)
    }

    println(s"METHOD-DELARATION-CODE-ELEMENTS: ${methodInstructions.size}")

    val globalCodeBlockBlock: NewBlock = vAstCreator.emptyBlockNodeHelper(superCVAst, Option(42), Option(42))
    val globalCodeBlock: Ast = vAstCreator.blockAstHelper(globalCodeBlockBlock, methodInstructions.toList)
    val methodNode = NewMethod()
      .name(superCVAst.getNode(0).getNode(1).getNode(0).getNode(0).getString(0))
      .filename(vAstCreator.getCurrentFilename)
      .code("<test-emthod>")
      .fullName(s"${vAstCreator.getCurrentFilename}:<test-emthod>")
      .lineNumber(2)
      .columnNumber(2)
    val returnStatement: NewMethodReturn = vAstCreator.methodReturnNodeHelper(superCVAst, "TEST")
      .lineNumber(2)
      .columnNumber(2)

    val method: Ast = vAstCreator.methodAstHelper(
      methodNode,
      List(),
      globalCodeBlock,
      returnStatement,
      modifiers = List()
    )
    Option(Seq(method))
  }
  
  private def getCode(returnType: String,
                      functionName: String,
                      parameters: Seq[(String, String)],
                      functionCode: String): String = {
    val parameterString: String = parameters.map((paramType, paramName) => s"${paramType} ${paramName}").mkString(", ")
    val codeBlock: String = functionCode.replace("\n", s"\n${this.BLOCK_SPACING}")
    s"${returnType} ${functionName}(${parameterString}) {\n${codeBlock}\n}"
  }
}
