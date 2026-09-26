package io.joern.c2cpg.astcreation.converter

import io.joern.c2cpg.astcreation.{Defines, VAstCreatorNew}
import io.joern.x2cpg.Ast
import io.shiftleft.codepropertygraph.generated.nodes.NewIdentifier
import superc.core.Syntax
import xtc.tree.{Location, Node}

import scala.collection.mutable

/**
 * This is a helper class to simplify the handling of parameters and variable declarations.
 */
class VAstVariableHandler(vAstCreator: VAstCreatorNew, converter: VAstConverter)
  extends VAstHandler(vAstCreator, converter) {

  private val logicHandler: VAstLogicHandler = converter.getLogicHandler
  private val conditionalHandler: VAstConditionalHandler = converter.getConditionalHandler

  private val SIMPLE_PARAMETER_DECLARATION: String = "SimpleDeclarator"
  private val ARRAY_PARAMETER_DECLARATION: String = "ArrayDeclarator"
  private val ARRAY_DIMENSION_PARAMETER: String = "ArrayAbstractDeclarator"
  private val POINTER_PARAMETER_DECLARATION: String = "UnaryIdentifierDeclarator"

  /**
   * Defines the internal variable scop data structure with the already created the global variable scope.
   *
   * The data structure is a sequence of variable scopes that are defines by methode declarations and code blocks. For
   * each variable  scopt one variable scope map exist in the sequence
   * Seq<variable scopes>(Map[<variable name>, Map[<presence condition>,<variable type>]]]
   *
   * @return Retruens the intial variable scope data structure with the already defined global variable scope.
   */
  override def getInitialConverterState: Any = Seq(mutable.Map.empty[String,mutable.Map[String, String]])

  /**
   * Handles the extraction of the function parameter type or variable type extraction and pass the annotated type information to the handle method together with the logcation information and the type root node
   * 
   * Important:
   * This Method calls the provided handler methode for each conditional annotated type seperatly.
   * 
   * @param rootTypeNode
   * @param converterState
   * @param handler
   * @return
   */
  def handleVariableType(rootTypeNode: Node, converterState: VAstConverterState,
                         handler: (VAstConverterState, String, Option[Int], Option[Int]) => Seq[Ast]): Seq[Ast] = {

    conditionalHandler.handleAndSimplifyConditionalExtended(rootTypeNode, converterState, (typeNode: Node, state: VAstConverterState) => {
      if (typeNode.isInstanceOf[Syntax]) {
        // If the type is only a simple data type (e.g. int, float, ...) without annotation (e.g. const, unsigned, ...)
        val (line: Option[Int], column: Option[Int]) = getLocation(typeNode)
        val parameterType: String = typeNode.getString(0)

        // Calls the provide handler with the parameter/variable type and the corresponding location information.
        handler(state, parameterType, line, column)

      } else {
        // If the type is a struct, an enum or an annotated type.
        handleComplexType(typeNode, state, handler)
      }
    })
  }

  private def handleComplexType(typeSpecifierNode: Node, converterState: VAstConverterState,
                                handler: (VAstConverterState, String, Option[Int], Option[Int]) => Seq[Ast]): Seq[Ast] = {
    typeSpecifierNode.getName match {
      case annotationRoot if annotationRoot.equals("BasicTypeSpecifier")
            || annotationRoot.equals("BasicDeclarationSpecifier") =>
        // If it is a simple type with a const annotation.
        val annotationRootNode: Node = typeSpecifierNode.getNode(0)
        val parameterTypeRootNode: Node = typeSpecifierNode.getNode(1)
        // Handles the simple parameter type.
        conditionalHandler.handleAndSimplifyConditionalExtended(parameterTypeRootNode, converterState,
                                                                (parameterTypeNode: Node, parameterTypeState: VAstConverterState) => {
          // Determines the simple parameter/variable type (e.g. "int", "float").
          val parameterType: String = parameterTypeNode.getString(0)

          // Handle the type annotation.
          conditionalHandler.handleAndSimplifyConditionalExtended(annotationRootNode, parameterTypeState,
                                                                  (annotationNode: Node, annotationState: VAstConverterState) => {
            handleTypeAnnotation(annotationNode, annotationState, handler, parameterType)
          })
        })

      case structOrEnumRoot if structOrEnumRoot.equals("SUETypeSpecifier")
            || structOrEnumRoot.equals("SUEDeclarationSpecifier") =>
        // If the type is a struct or an enum
        if (typeSpecifierNode.size == 1) {
          // If ghe type is a simple struct or an enum.
          val structOrEnumRootNode: Node = typeSpecifierNode.getNode(0)
          conditionalHandler.handleAndSimplifyConditionalExtended(structOrEnumRootNode, converterState,
                                                                  (typeRootNode: Node, typeState: VAstConverterState) => {
            handleStructAndEnumType(typeRootNode, typeState, handler)
          })

        } else {
          // If the type is a struct or an enum with an additional type annotation.
          val annotationNode: Node = typeSpecifierNode.getNode(0)
          val structOrEnumRootNode: Node = typeSpecifierNode.getNode(1)
          conditionalHandler.handleAndSimplifyConditionalExtended(structOrEnumRootNode, converterState,
                                                                  (typeRootNode: Node, typeState: VAstConverterState) => {
            // Handles the struct or enum type.
            handleStructAndEnumType(typeRootNode, typeState, (state: VAstConverterState, parameterType: String, line: Option[Int], column: Option[Int]) => {
              // Handles the type annotation.
              conditionalHandler.handleAndSimplifyConditionalExtended(annotationNode, state,
                (node: Node, annotationState: VAstConverterState) => {
                  handleTypeAnnotation(node, annotationState, handler, parameterType)
                })
            })
          })
        }
    }
  }

  private def handleStructAndEnumType(typeSpecifierNode: Node, converterState: VAstConverterState,
                                      handler: (VAstConverterState, String, Option[Int], Option[Int]) => Seq[Ast]): Seq[Ast] = {
    val dataTypeAnnotationNode: Node = typeSpecifierNode.getNode(0)
    val dataTypeNameRootNode: Node = typeSpecifierNode.getNode(1)

    // Determines the data type annotation and the code position.
    val dataTypeAnnotation: String = dataTypeAnnotationNode.getString(0)
    val (line: Option[Int], column: Option[Int]) = getLocation(dataTypeAnnotationNode)

    // Handles the data type name.
    conditionalHandler.handleAndSimplifyConditionalExtended(dataTypeNameRootNode, converterState,
                                                            (dataTypeNameNode: Node, dataTypeNameState: VAstConverterState) => {
      // Creates the type name.
      val dataTypeName: String = dataTypeNameNode.getNode(0).getString(0)
      val parameterType: String = dataTypeAnnotation + " " + dataTypeName

      handler(dataTypeNameState, parameterType, line, column)
    })
  }

  private def handleTypeAnnotation(typeAnnotationNode: Node, converterState: VAstConverterState,
                                   handler: (VAstConverterState, String, Option[Int], Option[Int]) => Seq[Ast],
                                   parameterType: String): Seq[Ast] = {
    // Selects the node with the annotation information
    val annotations: Node | Seq[Ast] = typeAnnotationNode match {
      case annotationNode: Syntax => annotationNode // If it is a type annotation e.g. "unsigned" or "signed".
      case constRootNode if constRootNode.getName.equals("TypeQualifierList") =>
        // If the type is marked as constant.
        // The conditional node duplicates the parent condition, so this conditional can be skipped.
        conditionalHandler.getFirstSuperCConditionalSubtree(constRootNode.getNode(0)).getNode(0).getNode(0)

      case externRootNode if externRootNode.getName.equals("DeclarationQualifierList") =>
        // If the type is marked as extern.
        // The conditional node duplicates the parent condition, so this conditional can be skipped.
        conditionalHandler.getFirstSuperCConditionalSubtree(externRootNode.getNode(0))

      case rootAnnotationNode if rootAnnotationNode.getName.equals("BasicTypeSpecifier")
            || rootAnnotationNode.getName.equals("BasicDeclarationSpecifier") =>
        // If the type annotation consists of multiple annotation instructions, such as “const unsigned”.
        // Handles the second/last annotation instruction.
        conditionalHandler.handleAndSimplifyConditionalExtended(rootAnnotationNode.getNode(1), converterState,
                                                                (annotationInstructionNode: Node, annotationInstructionState: VAstConverterState) => {
          // Extends the parameter/variable type by the second/last annotation.
          val secondTypeAnnotation: String = annotationInstructionNode.getString(0)
          val annotatedParameterType: String = secondTypeAnnotation + " " + parameterType

          // Handles the first/previus annotation instruction.
          conditionalHandler.handleAndSimplifyConditionalExtended(rootAnnotationNode.getNode(0), annotationInstructionState,
                                                                  (node: Node, state: VAstConverterState) => {
            handleTypeAnnotation(node, state, handler, annotatedParameterType)
          })
        })
    }

    annotations match {
      case annotationNode: Node =>
        // If an annotation SuperC node is returned.
        // creates the parameter/variable type information and determines the location information.
        val annotatedParameterType: String = annotationNode.getString(0) + " " + parameterType
        val (line: Option[Int], column: Option[Int]) = getLocation(annotationNode)

        // Calls the passed handler with the determined type and location information.
        handler(converterState, annotatedParameterType, line, column)

      case _ =>
        // If an JOERN AST sequence is returned with already transformed parameter/variable declarations.
        annotations.asInstanceOf[Seq[Ast]]
    }
  }

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
            conditionalHandler.handleAndSimplifyConditionalExtended(node.getNode(1), parameterNameState,
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
    registerVariableType(parameterName, fullParameterType, converterState) // Registers the variable declaration.
    parameterCreator(nameNode, converterState, fullParameterType, parameterName, code)
  }


  def handleVariableUse(primaryIdentifierNode: Node, converterState: VAstConverterState): Seq[Ast] = {
    converter.getConditionalHandler
      .handleAndSimplifyConditionalExtended(primaryIdentifierNode, converterState, handleVariableNode)
  }

  private def handleVariableNode(primaryIdentifierNode: Node, converterState: VAstConverterState): Seq[Ast] = {
    val conditionalHandler: VAstConditionalHandler = converter.getConditionalHandler
    val conditionalVariableNameNode: Node = primaryIdentifierNode.getNode(0)
    conditionalHandler.handleAndSimplifyConditionalExtended(conditionalVariableNameNode, converterState, (variableNameNode: Node, state: VAstConverterState) => {
      // Extracts the variable information
      val variableName: String = variableNameNode.getString(0)
      val (line: Option[Int], column: Option[Int]) = getLocation(variableNameNode)

      // Determines the variable type.
      val variableTypes: Seq[(String, String)] = getVariableType(variableName, state)

      // Creates the conditional variable nodes.
      val variableIdentifierNodes: Seq[(String, Ast)] = variableTypes.map((condition, fullVariableType: String) => {
        val variableIdentifierNode: NewIdentifier = NewIdentifier()
          .name(variableName)
          .code(variableName)
          .typeFullName(fullVariableType)
          .lineNumber(line)
          .columnNumber(column)

        (condition, vAstCreator.AstHelper(variableIdentifierNode))
      })
      Seq(conditionalHandler.createJoernMultiChoiceNode(primaryIdentifierNode, variableIdentifierNodes))
    })
  }

  private def getLocation(locationNode: Node): (Option[Int], Option[Int]) = {
    val location: Location = locationNode.getLocation
    if (location == null) (None, None) else (Option(location.line), Option(location.column))
  }

  /**
   * Returns all variable types along with their associated presence conditions that are defined for the requested
   * variable within the current variable scope if the presence condition can be satisfied at the current point in the
   * code.
   *
 *
   * @param variableName The variable name.
   * @param converterState The current converter state.
   * @return Retruns the sequence with all variable types along with their associated presence conditions that are
   *         defined for the requested variable within the current variable scope if the presence condition can be
   *         satisfied at the current point in the code.
   */
  private def getVariableType(variableName: String, converterState: VAstConverterState): Seq[(String, String)] = {
    // Retrieves the current condition.
    val currentCondition: String = conditionalHandler.getCurrentCondition(converterState)

    // Retrieves all variable type definition available in the current variable scope for the passed variable name.
    val variableScopes: Seq[mutable.Map[String,mutable.Map[String,String]]] = converterState.getState(this)
      .asInstanceOf[Seq[mutable.Map[String,mutable.Map[String,String]]]]

    // Creates all conditional variable type definitions with a satisfiable condition.
    variableScopes.last
      .getOrElse(variableName, mutable.Map.empty[String, String])
      .flatMap((condition: String, varType: String) => { // Filters only all satisfiable variable type definitions.
        val combinedCondition: String = logicHandler.combineAndSimplifyConditionsAnd(Seq(condition, currentCondition))
        if (logicHandler.isSatisfiable(combinedCondition)) Option((combinedCondition, varType)) else None
      })
      .toSeq
      .groupBy((condition: String, varType: String) => varType) // Groups all variable type definitions by variable type
      .map((varType: String, definitions: Seq[(String, String)]) => {
        val conditions: Seq[String] = definitions.map((vType: String, condition: String) => condition)
        val combinedCondition: String = logicHandler.combineAndSimplifyConditionsOr(conditions)
        (combinedCondition, varType)
      }).toSeq
  }

  /**
   * Registers the new variable type.
   *
   * @param variableName The name of the variable.
   * @param variableType The full variable type of the variable.
   * @param converterState The current converter state.
   */
  private def registerVariableType(variableName: String, variableType: String, converterState: VAstConverterState): Unit = {
    // Retrieves the current condition.
    val currentCondition: String = conditionalHandler.getCurrentCondition(converterState)

    val variableScopes: Seq[mutable.Map[String,mutable.Map[String,String]]] = converterState.getState(this)
      .asInstanceOf[Seq[mutable.Map[String,mutable.Map[String,String]]]]
    val variableTypes: mutable.Map[String,String] = variableScopes.last.getOrElse(variableName, mutable.Map.empty[String,String])
    val effectedConditions: Seq[(String,String, String)] = variableTypes.flatMap((condition: String, varType:  String) => {
      val newCondition: String = logicHandler.excludeConditionAndSimplify(condition, currentCondition)
      if (newCondition.equals(condition)) None else Option((condition, newCondition, varType))
    }).toSeq
    effectedConditions.foreach((oldCondition: String, newCondition: String, varType: String) => {
      variableTypes.remove(oldCondition)
      if (logicHandler.isSatisfiable(newCondition)) variableTypes.addOne(newCondition, varType)
    })
    variableTypes.addOne(currentCondition, variableType)
  }

  def addNewVariableScop(converterState: VAstConverterState): VAstConverterState = {
    val variableScope: Seq[mutable.Map[String,mutable.Map[String,String]]] = converterState.getState(this).asInstanceOf[Seq[mutable.Map[String,mutable.Map[String,String]]]]
    val updatedVariableScope: Seq[mutable.Map[String,mutable.Map[String,String]]] = variableScope ++ Seq(variableScope.last.clone())
    converterState.updateState(this, updatedVariableScope)
  }
}
