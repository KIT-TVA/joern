package io.joern.c2cpg.astcreation.converter

import io.joern.c2cpg.astcreation.VAstCreatorNew
import io.joern.x2cpg.Ast
import xtc.tree.{Location, Node}

/**
 * This is a helper class to simplify the handling of parameters and variable declarations.
 */
class VAstDeclarationHandler(vAstCreator: VAstCreatorNew, converter: VAstConverter)
  extends VAstHandler(vAstCreator, converter) {
  
  private val conditionalHandler: VAstConditionalHandler = converter.getConditionalHandler

  private val SIMPLE_PARAMETER_DECLARATION: String = "SimpleDeclarator"
  private val ARRAY_PARAMETER_DECLARATION: String = "ArrayDeclarator"
  private val ARRAY_DIMENSION_PARAMETER: String = "ArrayAbstractDeclarator"
  private val POINTER_PARAMETER_DECLARATION: String = "UnaryIdentifierDeclarator"

  /**
   *
   * @param parameterCreator SuperC node, VASTConverterState, variable name, full parameter name
   * @return
   */
  def handleDeclaration(nameNode: Node, parameterType: String, converterState: VAstConverterState,
                        parameterCreator: (Node, VAstConverterState, String, String, String) => Seq[Ast]): Seq[Ast] = {

    // Creates the importer parts of a parameter or variable
    conditionalHandler.handleAndSimplifyConditionalExtended(nameNode, converterState, (node: Node, state: VAstConverterState) => {
      handlePointInformation(node, state, parameterCreator, parameterType)
    })
  }

  private def handlePointInformation(pointerRootNode: Node, converterState: VAstConverterState,
                                     parameterCreator: (Node, VAstConverterState, String, String, String) => Seq[Ast],
                                     parameterType: String): Seq[Ast] = {
    // Extracts the pointer information.
    var pointerInformation: String = ""
    var nextPointerNode: Node = pointerRootNode
    while (nextPointerNode.getName.equals(POINTER_PARAMETER_DECLARATION)) {
      pointerInformation += "*"
      nextPointerNode = nextPointerNode.getNode(1)
    }

    // Handle parameter/variable name
    conditionalHandler.handleAndSimplifyConditionalExtended(nextPointerNode, converterState,
                                                            (node: Node, state: VAstConverterState) => {
      handleParameterName(node, state, parameterCreator, pointerRootNode, parameterType, pointerInformation)
    })
  }

  private def handleParameterName(node: Node, converterState: VAstConverterState,
                                  parameterCreator: (Node, VAstConverterState, String, String, String) => Seq[Ast],
                                  nameNode: Node, parameterType: String, pointerInformation: String) = {
    node.getName match {
      case nodeName if (nodeName.equals(SIMPLE_PARAMETER_DECLARATION)) =>
        val location: Location = node.getNode(0).getLocation
        val line: Option[Int] = Option(location.line)
        val column: Option[Int] = Option(location.column)
        val parameterName: String = node.getNode(0).getString(0)
        createNode(parameterType, parameterName, pointerInformation, "", nameNode, line, column,
                   parameterCreator, converterState)

      case nodeName if (nodeName.equals(ARRAY_PARAMETER_DECLARATION)) =>
        // Extracts the parameter name.
        conditionalHandler.handleAndSimplifyConditionalExtended(node.getNode(0), converterState,
                                                                (rootParameterNameNode: Node, parameterNameState: VAstConverterState) => {
          val parameterNameNode: Node = rootParameterNameNode.getNode(0)
          val location: Location = parameterNameNode.getLocation
          val line: Option[Int] = Option(location.line)
          val column: Option[Int] = Option(location.column)
          val parameterName: String = parameterNameNode.getString(0)

          // Determines the array information and creates the parameter nodes.
          conditionalHandler.handleAndSimplifyConditionalExtended(node.getNode(1), converterState,
                                                                  (n: Node, state: VAstConverterState) => {
            handleArrayDimensions(n, state, parameterCreator, nameNode, parameterType, pointerInformation,
                                  parameterName, "", line, column)
          })
        })
    }
  }

  private def handleArrayDimensions(arrayDimensionNode: Node, converterState: VAstConverterState,
                                    parameterCreator: (Node, VAstConverterState, String, String, String) => Seq[Ast],
                                    nameNode: Node, parameterType: String, pointerInformation: String,
                                    parameterName: String, arrayDimensionInformation: String,
                                    line: Option[Int], column: Option[Int]): Seq[Ast] = {

    arrayDimensionNode.size match {
      case nChildren if nChildren == 2 =>
        val nextArrayDimensionNode: Node = arrayDimensionNode.getNode(0)
        val arrayDimensionSizeNode: Node = arrayDimensionNode.getNode(1)
        conditionalHandler.handleAndSimplifyConditionalExtended(arrayDimensionSizeNode, converterState,
                                                                (dimensionSizeNode: Node, dimSizeState: VAstConverterState) => {
          // Handles the dimension size of the current array dimension.
          val extendedArrayDimensionInformation: String = s"[${dimensionSizeNode.getString(0)}]" + arrayDimensionInformation
          conditionalHandler.handleAndSimplifyConditionalExtended(nextArrayDimensionNode, dimSizeState,
                                                                  (dimensionNode: Node, state: VAstConverterState) => {
            // Handles the next array dimension.
            handleArrayDimensions(dimensionNode, state, parameterCreator, nameNode, parameterType, pointerInformation,
                                  parameterName, extendedArrayDimensionInformation, line, column)
          })
        })

      case nChildren if nChildren == 1 =>
        // If the first array dimension definition is reached and a dimension size is specified for this dimension or an
        // array dimension without a specified dimension size is found that is not the first array dimension.
        conditionalHandler.handleAndSimplifyConditionalExtended(arrayDimensionNode.getNode(0), converterState,
                                                                (node: Node, state: VAstConverterState) => {
          if (node.getName.equals(ARRAY_DIMENSION_PARAMETER)) {
            // Handles the next array dimension.
            val extendedArrayDimensionInformation: String = "[]" + arrayDimensionInformation
            handleArrayDimensions(node, state, parameterCreator, nameNode, parameterType, pointerInformation,
                                  parameterName, extendedArrayDimensionInformation, line, column)

          } else {
            // Creates the JOERN parameter/variable node.
            val finalVariableArrayInformation: String = s"[${node.getString(0)}]" + arrayDimensionInformation
            createNode(parameterType, parameterName, pointerInformation, finalVariableArrayInformation, nameNode,
                       line, column, parameterCreator, converterState)
          }
        })

      case _ =>
        // If the first array dimension definition is reached and no dimension size is specified for this dimension.
        val finalVariableArrayInformation: String = "[]" + arrayDimensionInformation
        createNode(parameterType, parameterName, pointerInformation, finalVariableArrayInformation, nameNode,
                   line, column, parameterCreator, converterState)
    }
  }

  private def handleArrayDimensionsOrg(node: Node, converterState: VAstConverterState,
                                    parameterCreator: (Node, VAstConverterState, String, String, String) => Seq[Ast],
                                    nameNode: Node, parameterType: String, pointerInformation: String,
                                    parameterName: String, arrayDimensionInformation: String,
                                    line: Option[Int], column: Option[Int]): Seq[Ast] = {
    var nextArrayNode: Node = node
    var variableArrayInformation: String = arrayDimensionInformation
    var isArrayNode: Boolean = true
    while (isArrayNode) {
      println(s"address auf array dim node: 0x${java.lang.Integer.toHexString(System.identityHashCode(node))}")
      variableArrayInformation = (nextArrayNode.size match {
        case nChildren if (nChildren == 2) => s"[${nextArrayNode.getNode(1).getString(0)}]"
        case nChildren if (nChildren == 1 && !nextArrayNode.getNode(0).getName.equals(ARRAY_DIMENSION_PARAMETER)) =>
          s"[${nextArrayNode.getNode(0).getString(0)}]"
        case _ => "[]"
      }) + variableArrayInformation
      if (nextArrayNode.size > 0 && (nextArrayNode.getNode(0).getName.equals(ARRAY_DIMENSION_PARAMETER)
        || conditionalHandler.isSuperCConditionalNode(nextArrayNode.getNode(0)))) {
        nextArrayNode = nextArrayNode.getNode(0)
        isArrayNode = !conditionalHandler.isSuperCConditionalNode(nextArrayNode)
      } else isArrayNode = false
    }

    if (conditionalHandler.isSuperCConditionalNode(nextArrayNode)) {
      conditionalHandler.handleAndSimplifyConditional(nextArrayNode, converterState, (arrayNode: Node, state: VAstConverterState) => {
        handleArrayDimensions(arrayNode, state, parameterCreator, nameNode, parameterType, pointerInformation,
                              parameterName, variableArrayInformation, line, column)
      })
    } else {
      createNode(parameterType, parameterName, pointerInformation, variableArrayInformation, nameNode, line, column,
                 parameterCreator, converterState)
    }
  }


  private def createNode(parameterType: String, parameterName: String, pointerInformation: String,
                         arrayDimensionInformation: String, nameNode: Node, line: Option[Int], column: Option[Int],
                         parameterCreator: (Node, VAstConverterState, String, String, String) => Seq[Ast],
                         converterState: VAstConverterState): Seq[Ast] = {
    val fullParameterType: String = parameterType + arrayDimensionInformation + pointerInformation
    val code: String = s"$parameterType$pointerInformation $parameterName$arrayDimensionInformation"
    parameterCreator(nameNode, converterState, fullParameterType, parameterName, code)
  }
}
