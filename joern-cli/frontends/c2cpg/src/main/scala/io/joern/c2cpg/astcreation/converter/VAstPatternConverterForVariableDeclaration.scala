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

  private val conditionalHandler: VAstConditionalHandler = converter.getConditionalHandler
  private val variableHandler: VAstVariableHandler = converter.getDeclarationHandler

  override def getInitialConverterState: Any = Seq.empty[(Node, Node)]

  override def convert(superCVAst: Node, converterState: VAstConverterState): Option[Seq[Ast]] = {
    val astSubtree: Seq[Ast] = superCVAst.getName match {
      case "Declaration" =>
        // Special treatment of the variable declaration root node.
        val declarationNode: Node = superCVAst.getNode(PREVIOUS_VARIABLE_DECLARATION)
        conditionalHandler.handleAndSimplifyConditionalExtended(declarationNode, converterState,
            (node: Node, state: VAstConverterState) => converter.convert(node, state))

      case "DeclaringList" => createDeclarations(superCVAst, converterState: VAstConverterState)
    }

    Option(astSubtree)
  }

  private def createDeclarations(declarationNode: Node, converterState: VAstConverterState): Seq[Ast] = {

    // Collects all consecutive variable declarations that have the same conditions.
    val newDeclarations: ListBuffer[(Node, Node)] = ListBuffer.empty[(Node, Node)] // ListBuffer[(<variable node root (with pointer and array information)>, <initialization root node>)]
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

    // Extends the pending variable declaration list.
    val previousDeclarations: Seq[(Node, Node)] = converterState.getState(this).asInstanceOf[Seq[(Node, Node)]] // ListBuffer[(<variable node root (with pointer and array information)>, <initialization root node>)]
    val allDeclarations: Seq[(Node, Node)] = previousDeclarations ++ newDeclarations.toSeq // ListBuffer[(<variable node root (with pointer and array information)>, <initialization root node>)]

    if (conditionalHandler.isSuperCConditionalNode(currentDeclarationNode)) {
      // If the next node is a conditional.
      // Prepares and performances the conditional handling.
      val newConverterState: VAstConverterState = converterState.updateState(this, allDeclarations)
      conditionalHandler.handleAndSimplifyConditional(currentDeclarationNode, newConverterState, createDeclarations)

    } else {
      // If the variable types node is reached.
      // Defines all declarations and initializations.
      variableHandler.handleVariableType(currentDeclarationNode, converterState,
        (variableTypeState: VAstConverterState, variableType: String, line: Option[Int], column: Option[Int]) => {
          allDeclarations.flatMap((variableNameNode: Node, initialisationNode) => {
            createVariableDeclaration(variableType, variableNameNode, initialisationNode, variableTypeState)
          })
        })

      /**
      val variableType: String = currentDeclarationNode.getString(0)
      allDeclarations.flatMap((variableNameNode: Node, initialisationNode) => {
        if (conditionalHandler.isSuperCConditionalNode(variableNameNode)) {
          conditionalHandler.handleAndSimplifyConditional(variableNameNode, converterState,
            (variableNode, state) => createVariableDeclaration(variableType, variableNode, initialisationNode, state))
        } else {
          createVariableDeclaration(variableType, variableNameNode, initialisationNode, converterState)
        }
      })
      **/
    }
  }

  private def createVariableDeclaration(variableType: String, variableNameNode: Node, initialisationNode: Node,
                                        converterState: VAstConverterState): Seq[Ast] = {
    conditionalHandler.handleAndSimplifyConditionalExtended(variableNameNode, converterState,
      (variableNode: Node, variableState: VAstConverterState) => {
      variableHandler.handleDeclarationWithLocation(variableNode, variableType, variableState,
        (variableNameRootNode: Node, converterState: VAstConverterState, variableNameNode: Node, fullVariableType: String, variableName: String, code: String, line: Option[Int], column: Option[Int]) => {
          createVariableDeclarationNode(variableNameRootNode, converterState, variableNameNode, fullVariableType,
                                        variableName, code, line, column, initialisationNode)
        })
    })
  }

  private def createVariableDeclarationNode(variableNameRootNode: Node, converterState: VAstConverterState,
                                            variableNameNode: Node, fullVariableType: String, variableName: String,
                                            code: String, line: Option[Int], column: Option[Int],
                                            initialisationRootNode: Node): Seq[Ast] = {
    val declaration: NewLocal = vAstCreator.localNodeHelper(variableNameRootNode, variableName, code, fullVariableType,
                                                            line=line, column=column)
    val declarationAst: Ast = vAstCreator.AstHelper(declaration)
    val initializationAsts: Seq[Ast] = createInitializationAst(initialisationRootNode, converterState, variableNameNode)

    Seq(declarationAst) ++ initializationAsts
  }

  private def createInitializationAst(initialisationRootNode: Node, converterState: VAstConverterState,
                                      variableNameNode: Node): Seq[Ast] = {
    if (initialisationRootNode.size == 0) {
      // If it is only a variable declaration.
      Seq.empty[Ast]

    } else {
      // If it is a variable declaration with initialization.

      conditionalHandler.handleAndSimplifyConditionalExtended(initialisationRootNode, converterState,
        (initialisationNode: Node, initializationState: VAstConverterState) => {
          // Converts the initialization node into an assignment node, because JOERN does not distinguish between
          // initialization and assignment.
          val targetVariableNode: Node = GNode.create(TARGET_VARIABLE_NODE_NAME, variableNameNode.getNode(0)) // Creates the primary variable identifier node.
          val assignmentOperatorNode: Node = GNode.create(ASSIGNMENT_OPERATOR_NODE_NAME) // Creates the
          val assignmentExpression: Node = initializerExpression(initialisationNode)
          val assignmentNode: Node = GNode.create(ASSIGNMENT_EXPRESSION_NODE_NAME, targetVariableNode,
                                                  assignmentOperatorNode, assignmentExpression)

          // Creates the initialization sub AST.
          converter.convert(assignmentNode, converterState)
        })
    }
  }

  /**
   * SuperC: initializer is either the expression itself (e.g. s.x, a * b) or wrapped in Initializer.
   */
  private def initializerExpression(initialisationNode: Node): Node =
    if (initialisationNode.getName == "Initializer" && initialisationNode.size() > 0) {
      initialisationNode.getNode(0)
    } else {
      initialisationNode
    }
}
