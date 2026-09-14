package io.joern.c2cpg.astcreation.converter

import io.joern.c2cpg.astcreation.{Defines, VAstCreatorNew}
import io.joern.x2cpg.Ast
import io.shiftleft.codepropertygraph.generated.nodes.{AstNodeNew, NewIdentifier, NewLiteral, NewNode, NewReturn}
import xtc.tree.{Location, Node}

class VAstPatternConverterForReturn(vAstCreator: VAstCreatorNew, converter: VAstConverter)
  extends VAstPatternConverter(vAstCreator, converter, List("ReturnStatement")) {

  private val conditionalHandler: VAstConditionalHandler = converter.getConditionalHandler

  override def convert(superCVAst: Node, converterState: VAstConverterState): Option[Seq[Ast]] = {
    
    // Generates the return content AST, e.g. the returned variable or expression to calculate the return value.
    val returnContentAst: Ast = getReturnContentAst(superCVAst, converterState)

    // Approximates the location information of the return statement if the root node of Return-Content-AST contains
    // location information. This approximation is required because the SuperC VAST does not provide location
    // information for the return instruction/node. Therefore, the information has to be estimated or determined by
    // pattern matching on the code file. Determining location information from the code file is possible but not
    // straightforward because the SuperC VAST does not preserve location order in all situations when conditional
    // macros are used. In addition, a return instruction can appear multiple times in the SuperC VAST when conditional
    // macros are used. These multiple appearances can often be identified by checking whether the return node object
    // instance is the same, but there may be conditional situations where this property is not conserved.
    //
    // As a result, the JOERN return node never contains location information when the return instruction is in a void
    // method, so that no information has to be returned or the return data argument/instruction/s is conditional.
    //
    // Important: The approximated location information can in some situation differ by one line depending on the file
    //            formation if location information are added to the return node.
    val (line: Option[Int], column: Option[Int]) = returnContentAst.root match {
      case Some(rootNode) =>
        val rootAstNode: AstNodeNew = rootNode.asInstanceOf[AstNodeNew]
        (rootAstNode.lineNumber, rootAstNode.columnNumber.map((column: Int) => column - 7))
      case None => (None, None)
    }

    // Generates the code.
    val code: String = returnContentAst.root match {
      case Some(rootNode) => s"return ${returnContentAst.root.get.asInstanceOf[AstNodeNew].code};"
      case None => "return;"
    }

    // Creates the return node and the corresponding AST.
    val returnNode: NewReturn = vAstCreator.returnNodeHelper(superCVAst, code, line, column)
    val returnAst: Ast = vAstCreator.AstHelper(returnNode).withChild(returnContentAst)
    Option(Seq(returnAst))
  }

  private def getReturnContentAst(returnNode: Node, converterState: VAstConverterState): Ast = {
    if (returnNode.size == 1) {
      val returnContentNode: Node = returnNode.getNode(0)
      returnContentNode.getName match {
        case "ExpressionOpt" => vAstCreator.AstHelper()
        case "PrimaryIdentifier" => variableAst(returnContentNode)
        case name if name.startsWith("superc.core.Syntax$") => constantAst(returnContentNode)
        case name if conditionalHandler.isSuperCConditionalNode(returnContentNode) =>
          conditionalHandler.handleConditional(returnContentNode, converterState, (node: Node, state: VAstConverterState) => {
            Seq(getReturnContentAst(node, state))
          }).head

        case _ =>
          val subasts: Seq[Ast] = converter.convert(returnContentNode, converterState)
          if (subasts.isEmpty) vAstCreator.AstHelper() else subasts.head
      }
    } else vAstCreator.AstHelper()
  }

  private def variableAst(variableNode: Node): Ast = {
    val variableNameNode: Node = variableNode.getNode(0)
    val variableName = variableNameNode.getString(0)
    val (line, column) = getLocation(variableNameNode)
    
    // Creates the variable node.
    val variableIdentifyerNode = NewIdentifier()
      .name(variableName)
      .code(variableName)
      .typeFullName(Defines.Any) // Tht variable type can be determined but this is not strait forward because the variable type can be depended on conditional macros. The possible variable types are given by the conditional dependencies of the return statements and the variable declaration.
      .lineNumber(line)
      .columnNumber(column)
    vAstCreator.AstHelper(variableIdentifyerNode)
  }

  private def constantAst(constantNode: Node): Ast = {
    val constant: String = constantNode.getString(0)
    
    // Decimetres/Guesses the type of the constant.
    // The type should be determined by the return type. A return type compatability check should be done to ensure validity e.g. char and float are not compatible.
    val typeFullName: String = constant match {
      case c if c.toBooleanOption.isDefined => "bool"
      case c if c.toByteOption.isDefined    => "byte"
      case c if c.toShortOption.isDefined   => "short"
      case c if c.toIntOption.isDefined     => "int"
      case c if c.toLongOption.isDefined    => "long"
      case c if c.toFloatOption.isDefined   => "float"
      case c if c.toDoubleOption.isDefined  => "double"
      case _                                => "char"
    }

    // Creates the constant node.
    val (line, column) = getLocation(constantNode)
    val literal = NewLiteral().code(constant).typeFullName(typeFullName).lineNumber(line).columnNumber(column)
    vAstCreator.AstHelper(literal)
  }

  private def getLocation(locationNode: Node): (Option[Int], Option[Int]) = {
    val  location: Location = locationNode.getLocation
    if (location == null) (None, None) else (Option(location.line), Option(location.column))
  }
}
