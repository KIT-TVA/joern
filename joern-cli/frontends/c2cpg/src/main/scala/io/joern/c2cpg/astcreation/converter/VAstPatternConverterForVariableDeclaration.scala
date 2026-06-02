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
  private val CONDITIONAL_NODE_NAME: String = "Conditional"

  private val ASSIGNMENT_EXPRESSION_NODE_NAME: String = "AssignmentExpression"
  private val ASSIGNMENT_OPERATOR_NODE_NAME: String = "AssignmentOperator"
  private val TARGET_VARIABLE_NODE_NAME: String = "PrimaryIdentifier"

  override def convert(superCVAst: Node): Option[Seq[Ast]] = {
    val astSubtree: Seq[Ast] = superCVAst.getName match {
      case "Declaration" => converter.convert(superCVAst.getNode(0))
      case "DeclaringList" => createDeclarations(superCVAst)
    }
    Option(astSubtree)
  }

  private def createDeclarations(declarationNode: Node): Seq[Ast] = {
    println(s"declaration Node type: ${declarationNode.getClass.toString}")
    println(s"first Node: ${declarationNode.get(0).getClass.toString}")
    Console.flush()
    declarationNode match {
      case node if node.get(0).getClass.toString.equals(VARIABLE_TYPE_NODE_CLASS_NAME) =>
        // Creates a simple variable declaration or the first variable declaration of multi inline declaration.
        val variableType: String = declarationNode.getNode(0).getString(0)
        val variableNameNode: Node = declarationNode.getNode(1)
        val initNode: Node = declarationNode.getNode(4)
        createDeclaration(declarationNode, variableType, variableNameNode, initNode)

      case node if hastConditionalVariableType(node) =>
        // Converts the variable declaration with conditional variable type into a normal variable declaration.
        convertConditionalVariableDeclarations(declarationNode)

      case node if node.getNode(0).getName.equals(VARIABLE_DECLARATION_NODE_NAME) =>
        // Create the variable declarations for multiple inline declarations.
        val declarations: Seq[Ast] = converter.convert(declarationNode.getNode(0))

        // Creates the next declaration.
        val variableType: String = declarations.head.root.get match {
          case variableTypeNode: NewLocal => variableTypeNode.typeFullName
        }
        val variableNameNode: Node = declarationNode.getNode(2)
        val initNode: Node = declarationNode.getNode(5)
        val newDeclarations: Seq[Ast] = createDeclaration(declarationNode, variableType, variableNameNode, initNode)
        declarations ++ newDeclarations
    }
  }

  private def hastConditionalVariableType(declarationNode: Node): Boolean = {
    var currentDeclarationNode: Node = declarationNode
    while (currentDeclarationNode.getName.equals(VARIABLE_DECLARATION_NODE_NAME)) {
      currentDeclarationNode = currentDeclarationNode.getNode(0)
    }
    currentDeclarationNode.getName.equals(CONDITIONAL_NODE_NAME)
  }

  private def createDeclaration(node: Node, variableType: String, variableNameNode: Node, initNode: Node): Seq[Ast] = {
    val variableName: String = variableNameNode.getNode(0).getString(0)

    val location: Location = variableNameNode.getNode(0).getLocation
    val line: Option[Int] = if (location == null) None else Option(location.line)
    val column: Option[Int] = if (location == null) None else Option(location.column)

    val code: String = s"$variableType $variableName"
    val declaration: NewLocal = vAstCreator.localNodeHelper(node, variableName, code, variableType,
                                                            line=line, column=column)
    val declarationAst: Ast = vAstCreator.AstHelper(declaration)

    if (initNode.size == 0) {
      // If it is only a variable declaration.
      Seq(declarationAst)

    } else {
      // If it is a variable declaration with initialization.
      val targetVariableNode: Node = GNode.create(TARGET_VARIABLE_NODE_NAME, variableNameNode.getNode(0))
      val assignmentOperatorNode: Node = GNode.create(ASSIGNMENT_OPERATOR_NODE_NAME)
      val assignmentNode: Node = GNode.create(ASSIGNMENT_EXPRESSION_NODE_NAME, targetVariableNode,
                                              assignmentOperatorNode, initNode.getNode(0))

      val assignmenAstSubtree: Ast = converter.convert(assignmentNode).head
      Seq(declarationAst, assignmenAstSubtree)
    }
  }

  private def convertConditionalVariableDeclarations(declarationNode: Node): Seq[Ast] = {
    val (rootDeclarationNode: Seq[Node], conditionalNode: Seq[AnyRef]) = copyDeclarations(declarationNode)

    val newSuperCSubtree: Node = GNode.create("Conditional", conditionalNode.size * 2)
    for (index: Int  <- conditionalNode.indices) {
      newSuperCSubtree.add(2 * index, conditionalNode(index))
      newSuperCSubtree.add(2 * index + 1, rootDeclarationNode(index))
    }
    converter.convert(newSuperCSubtree)
  }

  private def copyDeclarations(declarationNode: Node): (Seq[Node], Seq[AnyRef]) = {
    declarationNode.getNode(0).getName match {
      case VARIABLE_DECLARATION_NODE_NAME =>
        // If a previous declaration exists.
        val (newPreviousDeclaration: Seq[Node], conditions: Seq[AnyRef]) = copyDeclarations(declarationNode.getNode(0))
        val newDeclarations: Seq[Node] = newPreviousDeclaration.map(previousDeclaration =>
          GNode.create(VARIABLE_DECLARATION_NODE_NAME,
                       previousDeclaration,
                       declarationNode.getNode(1),
                       declarationNode.getNode(2),
                       declarationNode.getNode(3),
                       declarationNode.getNode(4),
                       declarationNode.getNode(5)))
        (newDeclarations, conditions)
      case _ =>
        // IF the conditional Node is reached.
        val conditionalNode: Node = declarationNode.getNode(0)

        var index: Int = 0
        val newDeclarationNodes: ListBuffer[Node] = ListBuffer.empty[Node]
        val conditions: ListBuffer[AnyRef] = ListBuffer.empty[AnyRef]
        println(s"conditional Node Size: ${conditionalNode.size}")
        while (index < conditionalNode.size) {
          println(s"current index: ${index}")
          conditions += conditionalNode.get(index)
          val newDeclarationNode: Node = GNode.create(VARIABLE_DECLARATION_NODE_NAME,
                                                      conditionalNode.getNode(index + 1),
                                                      declarationNode.getNode(1),
                                                      declarationNode.getNode(2),
                                                      declarationNode.getNode(3),
                                                      declarationNode.getNode(4))
          newDeclarationNodes += newDeclarationNode
          index += 2
        }
        (newDeclarationNodes.toSeq, conditions.toSeq)
    }
  }
}
