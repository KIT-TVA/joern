package io.joern.c2cpg.astcreation.converter

import io.joern.c2cpg.astcreation.VAstCreatorNew
import io.joern.x2cpg.Ast
import io.shiftleft.codepropertygraph.generated.nodes.NewLocal
import xtc.tree.{GNode, Location, Node}

import scala.collection.mutable.ListBuffer

class VAstPatternConverterForVariableDeclaration(vAstCreator: VAstCreatorNew, converter: VAstConverter)
  extends VAstPatternConverter(vAstCreator, converter, List.apply("Declaration", "DeclaringList")) {

  private val VARIABLE_DECLARATION_NODE_NAME: String = "DeclaringList"
  private val SIMPLE_VARIABLE_DECLARATION_NODE_CLASS_NAME: String = "class xtc.tree.GNode$Fixed4"
  private val MULTIPLE_VARIABLE_DECLARATION_NODE_CLASS_NAME: String = "class xtc.tree.GNode$Fixed5"
  private val VARIABLE_TYPE_NODE_CLASS_NAME: String = "class superc.core.Syntax$Language"
  // private val CONDITIONAL_NODE_NAME: String = "Conditional"

  private val ASSIGNMENT_EXPRESSION_NODE_NAME: String = "AssignmentExpression"
  private val ASSIGNMENT_OPERATOR_NODE_NAME: String = "AssignmentOperator"
  private val TARGET_VARIABLE_NODE_NAME: String = "PrimaryIdentifier"

  private val PREVIOUS_VARIABLE_DECLARATION: Int = 0

  private val FIRST_DECLARATION_NODE_NODE_SIZE: Int = 5

  override def getInitialState: Any = Seq.empty[(Node, Node)]

  override def convert(superCVAst: Node, converterState: VAstConverterState): Option[Seq[Ast]] = {
    val astSubtree: Seq[Ast] = superCVAst.getName match {
      case "Declaration" =>
        val declarationNode: Node = superCVAst.getNode(PREVIOUS_VARIABLE_DECLARATION)
        val conditionalHandler: VAstConditionalHandler = converter.getConditionalHandler
        if (conditionalHandler.isConditionalNode(declarationNode)) {
          conditionalHandler.handelAndSimplifyConditional(declarationNode, converterState,
            (node: Node, state: VAstConverterState) => converter.convert(node, state))
        } else {
          converter.convert(declarationNode, converterState)
        }
      case "DeclaringList" => createDeclarations(superCVAst, converterState: VAstConverterState)
    }

    Option(astSubtree)
  }

  private def createDeclarations(declarationNode: Node, converterState: VAstConverterState): Seq[Ast] = {

    // Collects all consecutive variable declarations that hare the same conditions.
    val newDeclarations: ListBuffer[(Node, Node)] = ListBuffer.empty[(Node, Node)]
    var currentDeclarationNode: Node = declarationNode
    while (currentDeclarationNode.getName.equals(VARIABLE_DECLARATION_NODE_NAME)) {
      // Extracts the variable name and the initialization.
      val declarationInformation: (Node, Node) = if (currentDeclarationNode.size == FIRST_DECLARATION_NODE_NODE_SIZE) {
        (currentDeclarationNode.getNode(1), currentDeclarationNode.getNode(4))
      } else {
        (currentDeclarationNode.getNode(2), currentDeclarationNode.getNode(5))
      }
      newDeclarations.prepend(declarationInformation)

      currentDeclarationNode = currentDeclarationNode.getNode(PREVIOUS_VARIABLE_DECLARATION)
    }

    // Extends the variable declaration list.
    val previousDeclarations: Seq[(Node, Node)] = converterState.getState(this).asInstanceOf[Seq[(Node, Node)]]
    val allDeclarations: Seq[(Node, Node)] = previousDeclarations ++ newDeclarations.toSeq

    val conditionalHandler: VAstConditionalHandler = converter.getConditionalHandler
    if (conditionalHandler.isConditionalNode(currentDeclarationNode)) {
      // If the next node is a conditional.
      // Prepares and performances the conditional handling.
      val newConverterState: VAstConverterState = converterState.updateState(this, allDeclarations)
      conditionalHandler.handelAndSimplifyConditional(currentDeclarationNode, newConverterState, createDeclarations)

    } else {
      // If the variable types node is reached.
      // Defines all declarations and initializations.
      val variableType: String = currentDeclarationNode.getString(0)
      allDeclarations.flatMap((variableNameNode: Node, initialisationNode) => {
        if (conditionalHandler.isConditionalNode(variableNameNode)) {
          conditionalHandler.handelAndSimplifyConditional(variableNameNode, converterState,
            (variableNode, state) => createVariableDeclaration(variableType, variableNode, initialisationNode, state))
        } else {
          createVariableDeclaration(variableType, variableNameNode, initialisationNode, converterState)
        }
      })
    }
  }

  private def createVariableDeclaration(variableType: String, variableNameNode: Node, initialisationNode: Node,
                                        converterState: VAstConverterState): Seq[Ast] = {
    val variableName: String = variableNameNode.getNode(0).getString(0)

    val location: Location = variableNameNode.getNode(0).getLocation
    val line: Option[Int] = if (location == null) None else Option(location.line)
    val column: Option[Int] = if (location == null) None else Option(location.column)

    val code: String = s"$variableType $variableName"
    val declaration: NewLocal = vAstCreator.localNodeHelper(variableNameNode, variableName, code, variableType,
                                                            line=line, column=column)
    val declarationAst: Ast = vAstCreator.AstHelper(declaration)

    if (initialisationNode.size == 0) {
      // If it is only a variable declaration.
      Seq(declarationAst)

    } else {
      // If it is a variable declaration with initialization.
      // Converts the initialization node into an assignment node, because JOERN does not distinguish between
      // initialization and assignment.
      val targetVariableNode: Node = GNode.create(TARGET_VARIABLE_NODE_NAME, variableNameNode.getNode(0))
      val assignmentOperatorNode: Node = GNode.create(ASSIGNMENT_OPERATOR_NODE_NAME)
      val assignmentNode: Node = GNode.create(ASSIGNMENT_EXPRESSION_NODE_NAME, targetVariableNode,
                                              assignmentOperatorNode, initialisationNode.getNode(0))

      Seq(declarationAst) ++ converter.convert(assignmentNode, converterState)
    }
  }
}
