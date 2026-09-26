package io.joern.c2cpg.astcreation.converter

import io.joern.c2cpg.astcreation.VAstCreatorNew
import io.joern.c2cpg.astcreation.converter.{VAstConverter, VAstPatternConverter}
import io.joern.x2cpg.{Ast, AstEdge}
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, nodes}
import io.shiftleft.codepropertygraph.generated.nodes.{AstNodeNew, NewBlock, NewControlStructure, NewMethod, NewMethodParameterIn, NewMethodRef, NewMethodReturn, NewNode}
import superc.core.PresenceConditionManager.PresenceCondition
import superc.core.Syntax
import xtc.tree.{GNode, Location, Node}

import scala.collection.JavaConverters.asScalaSetConverter
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class VAstPatternConverterForFunctionDeclaration(vAstCreator: VAstCreatorNew, converter: VAstConverter)
  extends VAstPatternConverter(vAstCreator, converter, List.apply("FunctionDefinition")) {
  private val logicHandler: VAstLogicHandler = converter.getLogicHandler
  private val conditionalHandler: VAstConditionalHandler = converter.getConditionalHandler
  private val variableHandler: VAstVariableHandler = converter.getDeclarationHandler

  private val FUNCTION_DECLARATION: Int = 0
  private val FUNCTION_CODE_INDEX: Int = 1
  
  private val FUNCTION_DEFINITION: String = "FunctionDefinition"
  private val FUNCTION_RETURN_TYPE_ROOT_NODE_NAME: String = "FunctionPrototype"
  private val FUNCTION_NAME_ROOT_NODE_NAME: String = "FunctionDeclarator"
  private val FUNCTION_PARAMETER_ROOT_NODE_NAME: String = "PostfixingFunctionDeclarator"
  private val FUNCTION_PARAMETER_LIST_NODE: String = "ParameterList"
  private val SIMPLE_PARAMETER_DECLARATION: String = "SimpleDeclarator"
  private val ARRAY_PARAMETER_DECLARATION: String = "ArrayDeclarator"
  private val ARRAY_DIMENSION_PARAMETER: String = "ArrayAbstractDeclarator"
  private val POINTER_PARAMETER_DECLARATION: String = "UnaryIdentifierDeclarator"

  private val JOERN_BLOCK_NODE_KIND: Short = 6
  private val JOERN_BLOCK_NODE_LABEL: String = "BLOCK"

  private val JOERN_METHOD_RETURN_NODE_KIND: Short = 29
  private val JOERN_JOERN_METHOD_RETURN_NODE_LABEL: String = "METHOD_RETURN"
  
  private val CHOICE_TYPE_SEPARATOR: String = ";"
  
  override def convert(superCVAst: Node, converterState: VAstConverterState): Option[Seq[Ast]] = {
    // Extracts all important parameters
    val functionPropertyRootNode: Node = superCVAst.getNode(0)
    val methodRootCodeNode: Node = superCVAst.getNode(1)
    val methodReturnTypeNode: Node = functionPropertyRootNode.getNode(0)
    val methodNameRootNode: Node = functionPropertyRootNode.getNode(1).getNode(0)
    val methodParameterListRootNode: Node = functionPropertyRootNode.getNode(1).getNode(1)

    // Checks if the methode name is Conditional.
    if (conditionalHandler.isSuperCConditionalNode(methodNameRootNode)) {
      // Translates the method declarations if the method name is conditional.
      // Transforms the SuperC method declaration VAST with conditional method name into a SuperC VAST with conditional
      // method declarations but unconditional method names.
      val newSubVAst: Node = conditionalHandler.createConditionalSuperCSubtree(methodNameRootNode, converterState,
                                                                               (methodNameNode: Node, state: VAstConverterState) => {
        val newFunctionDeclarator: Node = GNode.create(FUNCTION_NAME_ROOT_NODE_NAME, methodNameNode, methodParameterListRootNode)
        val newFunctionPropertyRootNode:  Node = GNode.create(FUNCTION_RETURN_TYPE_ROOT_NODE_NAME, methodReturnTypeNode, newFunctionDeclarator)
        GNode.create(FUNCTION_DEFINITION, newFunctionPropertyRootNode, methodRootCodeNode)
      })

      // Transforms the modified SuperC VAST into as JOERN VAST.
      Option(conditionalHandler.handleAndSimplifyConditional(newSubVAst, converterState,
        (node: Node, state: VAstConverterState) => converter.convert(node, state)))

    } else {
      // Translates the method declarations if the method name is not conditional.
      val methodName: String = functionPropertyRootNode.getNode(1).getNode(0).getNode(0).getString(0)

      // Converts the return type and determines the method Position
      val returnTypeRootNode: Node = functionPropertyRootNode.getNode(0)
      val (returnTypeNodes: Seq[Ast], returnTypeString: String, returnTypeCode: String, methodLine: Int, methodColumn: Int)
        = getFunctionReturn(returnTypeRootNode, converterState)

      // Translates the function parameters.
      val (parameterNodes: Seq[Ast], parameterSignatureString, parameterNodeCode) = getFunctionParameters(functionPropertyRootNode, converterState)

      // Translates the method instructions of the current method.
      val instructionSuperCRootNode: Node = superCVAst.getNode(1)
      val (codeBlockAst: Ast, codeBlockCode: String) =
        getMethodInstructionBlock(instructionSuperCRootNode, converterState)

      // Creates the method code and the generic method signature.
      val methodeCode: String = s"$returnTypeCode $methodName($parameterNodeCode) $codeBlockCode\n"
      val methodSignature: String = s"$returnTypeString($parameterSignatureString)"

      // Creates the methode root node
      val methodNode = NewMethod()
        .name(superCVAst.getNode(0).getNode(1).getNode(0).getNode(0).getString(0))
        .filename(vAstCreator.getCurrentFilename)
        .code(methodeCode)
        .fullName(methodName)
        .signature(methodSignature)
        .lineNumber(methodLine)
        .columnNumber(methodColumn)

      // Builds the method declaration VAST.
      val method: Ast = vAstCreator.methodAstHelper(
        methodNode,
        parameterNodes,
        codeBlockAst,
        returnTypeNodes,
        modifiers = List()
      )

      // Creates the methode declaration refernce node.
      val methodRefNode: NewMethodRef = vAstCreator.methodRefNodeHelper(superCVAst, methodName, methodName, methodName)
        .lineNumber(methodLine)
        .columnNumber(methodColumn)
      val methodRefAst: Ast = vAstCreator.AstHelper(methodRefNode)

      // Returns the method declaration and the method ref node as two ASTs.
      Option(Seq(method, methodRefAst))
    }
  }

  /**
   * Converts the method return type SuperC VAST into a JOERN VAST, determines the code position of the method
   * declaration and computes the return type code.
   *
   * @param returnTypeRootNode The SuperC root node of the method return type VAST.
   * @param converterState     The current converter state
   * @return Returns the method return type information as a tuple.
   *         (return type JOERN AST, return type (if required generic), return type code, method begin line, method, begin column)
   */
  private def getFunctionReturn(returnTypeRootNode: Node, converterState: VAstConverterState): (Seq[Ast], String, String, Int, Int) = {
    // Initial definition of general method node properties that the translation of the method return nodes will
    // retrieve on the fly.
    var methodLine: Int = Int.MaxValue
    var methodColumn: Int = Int.MaxValue
    var returnTypes: Set[String] = Set.empty[String]

    // Defines the method return tyoe node creator method.
    val createReturnTypeNodes: (Node, VAstConverterState) => Seq[Ast] = (returnTypeNode: Node, converterState: VAstConverterState) => {
      val returnTypeLine: Int = returnTypeNode.getLocation.line
      val returnTypeColumn: Int = returnTypeNode.getLocation.column
      val returnType: String = returnTypeNode.getString(0)
      returnTypes = returnTypes + returnType

      // Updates the code position of the method if the current code position does not point to the beginning of the
      // method. This can happen when the return type is conditional because, in some situations, SuperC does not
      // preserve the code-order of the return types in its conditional subtree.
      if (methodLine > returnTypeLine) {
        methodLine = returnTypeLine
        methodColumn = returnTypeColumn
      } else if ((methodLine == returnTypeLine) && (methodColumn > returnTypeColumn)) {
        methodColumn = returnTypeColumn
      }

      val returnTypeStatement: NewMethodReturn = vAstCreator.methodReturnNodeHelper(returnTypeNode, returnType)
        .lineNumber(returnTypeLine)
        .columnNumber(returnTypeColumn)
        .code(returnType)
      Seq(vAstCreator.AstHelper(returnTypeStatement))
    }

    // Converts all method return type node.
    val returnTypeNodes: Seq[Ast] = if (conditionalHandler.isSuperCConditionalNode(returnTypeRootNode)) {
      val returnNodes: Seq[Ast] = conditionalHandler.handleAndSimplifyConditional(returnTypeRootNode, converterState,
        createReturnTypeNodes)
      val typesList: String = returnNodes.map((returnAst: Ast) => {
        returnAst.root.get match {
          case methodReturnNode: NewMethodReturn => methodReturnNode.typeFullName
          case controlStructureNode: NewControlStructure => returnAst.edges
            .filter((edge: AstEdge) => edge.src == controlStructureNode)
            .map((edge: AstEdge) => edge.dst.asInstanceOf[NewMethodReturn].typeFullName).head
        }
      }).sorted.mkString(";")

      // Creates an additional generic/multitype return node. This is necessary because JOERN expects to have only a
      // single finale/return point with only single return type.
      val genericReturnType: String = s"choice[$typesList]"
      val genericReturnTypeCode: String = returnNodes.map((returnType: Ast) => returnType.root.get.properties("CODE"))
        .mkString("\n")
      val genericReturnNode: NewMethodReturn = vAstCreator.methodReturnNodeHelper(returnTypeRootNode, genericReturnType)
        .lineNumber(methodLine)
        .columnNumber(methodColumn)
        .code(genericReturnTypeCode)
      val genericReturnAst: Ast = vAstCreator.AstHelper(genericReturnNode)
      Seq(genericReturnAst.withChildren(returnNodes))

    } else createReturnTypeNodes(returnTypeRootNode, converterState)
    val returnTypeString: String = toTypeString(returnTypes)

    // Extracts the return type code and corrects the code field of the return node in the return-type JEORN VAST.
    val returnTypeCode: String = returnTypeNodes.head.root.get.asInstanceOf[NewMethodReturn].code
    returnTypeNodes.head.nodes
      .filter((node: NewNode) => (node.nodeKind == JOERN_METHOD_RETURN_NODE_KIND)
        && node.label.equals(JOERN_JOERN_METHOD_RETURN_NODE_LABEL))
      .foreach((node: NewNode) => node.asInstanceOf[NewMethodReturn].code("REF"))

    // Returns all method return information
    (returnTypeNodes, returnTypeString, returnTypeCode, methodLine, methodColumn)
  }

  private def toTypeString(types: Set[String]): String = if (types.size > 1) {
    s"choice[${types.toSeq.sorted.mkString(CHOICE_TYPE_SEPARATOR)}]"
  } else types.mkString

  /**
   * Converts the method parameter SuperC VAST into a JOERN VAST and computes the parameter defintion code.
   *
   * @param functionPropertyRootNode The SuperC root node of the method parameter VAST.
   * @param converterState     The current converter state
   * @return Returns the method parameter information as a tuple.
   *         (method parameter JOERN nodes, methpod parameter type signature (if required generic), method parameter code)
   */
  private def getFunctionParameters(functionPropertyRootNode: Node, converterState: VAstConverterState): (Seq[Ast], String, String) = {
    // Converts the parameters.
    val parameterTypeListNode: Node = functionPropertyRootNode.getNode(1).getNode(1).getNode(0)
    var parameterNodes: Seq[Ast] = Seq.empty[Ast]
    if (parameterTypeListNode.size > 0) { // Checks if the current method does have parameters
      // Defines the handler/method parameter extractor.
      val methodParameterExtractor: (Node, VAstConverterState) => Seq[Ast] = (node: Node, extractorState: VAstConverterState) => {

        // Defines the handler/method to create the all JOERN method parameter nodes.
        val parameterNameRootNode: Node = node.getNode(1)
        val methodParameterCreator: (Node, VAstConverterState) => Seq[Ast] = (parameterTypeNode: Node, state: VAstConverterState) => {
          val parameterType: String = parameterTypeNode.getString(0)
          val location: Location = parameterTypeNode.getLocation
          val (line: Option[Int], column: Option[Int]) =
            if (location == null) (None, None) else (Option(location.line), Option(location.column))

          // Creates each JOERN method parameter node.
          variableHandler.handleDeclaration(parameterNameRootNode, parameterType, state,
            (nameNode: Node, s: VAstConverterState, fullParameterType: String, parameterName: String, code: String) => {
              // The parameter index for each parameter Nod is set after all parameter nodes are translated and in the
              // right order because in some conditional situations the parameters in the SuperC AST may not in order.
              val parameterNode: NewMethodParameterIn = vAstCreator.parameterInNodeHelper(node, parameterName, code,
                -1, false, "BY_VALUE", fullParameterType, dynamicTypeHintFullName = Seq(), line = line, column = column)
              Seq(vAstCreator.AstHelper(parameterNode))
            })
        }

        val parameterTypeRootNode: Node = node.getNode(0)
        if (conditionalHandler.isSuperCConditionalNode(parameterTypeRootNode)) {
          conditionalHandler.handleAndSimplifyConditional(parameterTypeRootNode, extractorState, methodParameterCreator)
        } else methodParameterCreator(parameterTypeRootNode, extractorState)
      }

      // Extracts all method parameter.
      parameterNodes = conditionalHandler.handleAndSimplifyConditionalExtended(parameterTypeListNode, converterState,
                                                                               (typeListNode: Node, parameterTypeState: VAstConverterState) => {
        var conditionalParameterNodes: Seq[Ast] = Seq.empty[Ast]
        val parameterListNode: Node = typeListNode.getNode(0).getNode(0) // root node of all parameters (including the conditional ones)
        val numberOfParameters: Int = parameterListNode.size
        for (parameterNodeIndex: Int <- 0 until numberOfParameters) { // Iterates over all parameters (parameter nodes).
          val parameterNode: Node = parameterListNode.get(parameterNodeIndex).asInstanceOf[Node]

          // Extracts one method parameter.
          val newParameterNodes: Seq[Ast] = conditionalHandler.handleAndSimplifyConditionalExtended(parameterNode, parameterTypeState,
            (conditionalParameterNode: Node, parameterState: VAstConverterState) => {
              if (conditionalParameterNode.getName.equals(FUNCTION_PARAMETER_LIST_NODE)) {
                // If the SuperC node is a conditional "ParameterList" node (a second "ParameterList" node).
                var conditionalParameterAsts: Seq[Ast] = Seq.empty[Ast]
                val numberOfConditionalParameters: Int = conditionalParameterNode.size
                if (numberOfConditionalParameters > 0) {
                  for (conditionalParameterIndex: Int <- 0 until numberOfConditionalParameters) { // Iterates over a conditional subset parameters (parameter nodes) that share at least on condition.
                    val currentNode: Node = conditionalParameterNode.getNode(conditionalParameterIndex)
                    conditionalParameterAsts = conditionalParameterAsts
                      ++ conditionalHandler.handleAndSimplifyConditionalExtended(currentNode, parameterState,
                                                                                 handleOneFunctionParameter)
                  }
                }
                conditionalParameterAsts

              } else {
                // If the SuperC node is a "ParameterIdentifierDeclaration" node.
                handleOneFunctionParameter(conditionalParameterNode, parameterState)
              }
            })
          conditionalParameterNodes = conditionalParameterNodes ++ newParameterNodes
        }
        conditionalParameterNodes
      })
    }

    // Sorts the parameters by is position in the method signature.
    parameterNodes = parameterNodes.sortBy((parameterAst: Ast) => {
      var rootNode: NewNode = parameterAst.root.get
      if (conditionalHandler.isJoernChoiceNode(rootNode)) {
        rootNode = parameterAst.edges.filter((edge: AstEdge) => edge.src == rootNode).head.dst
      }
      val parameterNode: AstNodeNew = rootNode.asInstanceOf[AstNodeNew]
      val line: Int = parameterNode.lineNumber.get
      val column: Int = parameterNode.columnNumber.get
      (line, column)
    }).zipWithIndex.map((parameterAst: Ast, parameterIndex: Int) => {
      var rootNode: NewNode = parameterAst.root.get
      if (conditionalHandler.isJoernChoiceNode(rootNode)) {
        rootNode = parameterAst.edges.filter((edge: AstEdge) => edge.src == rootNode).head.dst
      }
      rootNode.asInstanceOf[NewMethodParameterIn].index(parameterIndex + 1)
      parameterAst
    })

    // Creates the parameter type signature.
    // Determines all possible parameter type orders.
    var parameterConfigurationMap: Map[String, Seq[String]] = Map.empty[String, Seq[String]]
    for (parameter: Ast <- parameterNodes) { // Iterates over all parameter- and conditional parameter-nodes.
      parameter.root.get match {
        case parameterNode: NewMethodParameterIn =>
          // If the parameter is unconditional.
          val newParamType: Seq[String] = Seq(parameterNode.typeFullName)
          if (parameterConfigurationMap.isEmpty) parameterConfigurationMap = Map("1" -> newParamType) else {
            parameterConfigurationMap = parameterConfigurationMap.map((condition: String, paramTypes: Seq[String]) =>
              (condition, paramTypes ++ newParamType))
          }

        case conditionalNode: NewControlStructure =>
          // If the parameter is conditional.
          val parameterCondition: String = conditionalHandler.getFirstJoernPresenceConditions(conditionalNode)
          val parameterNode: NewMethodParameterIn = parameter.edges
            .filter((edge: AstEdge) => edge.src.equals(conditionalNode)).head.dst.asInstanceOf[NewMethodParameterIn]
          val newParamType: Seq[String] = Seq(parameterNode.typeFullName)
          if (parameterConfigurationMap.isEmpty) {
            // If the first parameter is conditional.
            parameterConfigurationMap = Map("1" -> Seq("none"), parameterCondition -> newParamType)

          } else {
            // If the current parameter is not the first parameter.
            parameterConfigurationMap = parameterConfigurationMap.flatMap((condition: String, paramTypes: Seq[String]) => {
              // The condition comparison based on the conditional string is possible because the VAstConditionalHandler
              // ensures a deterministic order of the macro-variables in the conditional expressions.
              if (condition.contains(parameterCondition)) Seq((condition, paramTypes ++ newParamType)) // Condition already contained.
              else if (!combinedConditionSatisfiable(condition, parameterCondition)) Seq((condition, paramTypes)) // Combined condition not satisfiable.
              else Seq((condition, paramTypes), (s"$condition ;; $parameterCondition", paramTypes ++ newParamType)) // Combined condition satisfiable. | ";;" is a unique conditional separator, that is not part of an expression.
            })
          }
      }
    }

    // Removes any “none” parameter that is outdated.
    val removeCondition1: Boolean = if (parameterConfigurationMap.contains("1")
      && (parameterConfigurationMap("1").size > 1 || !parameterConfigurationMap("1").head.equals("none"))) false else {
      // If the condition "1" only contains Seq("none").
      // Checks if the method has at least one conditional configuration that has no parameters.
      val allTypeConditions: Seq[String] = parameterConfigurationMap.toSeq
        .flatMap((condition: String, parameterTypes: Seq[String]) => if (condition.equals("1")) None else {
          val allConditions: Seq[String] = condition.split(" ;; ").toSeq
          val combinedCondition: String = logicHandler.combineAndSimplifyConditionsAnd(allConditions)
          Option(combinedCondition)
        })
      val combinedTypeConditions: String = logicHandler.combineAndSimplifyConditionsOr(allTypeConditions)
      logicHandler.isTautology(combinedTypeConditions)
    }
    val parameterConfigurations: Seq[Seq[String]] = parameterConfigurationMap.toSeq
      .flatMap((condition: String, parameterTypes: Seq[String]) => {
        if (condition.equals("1") && removeCondition1) None // If the method always has at lest one parameter.
        else if (parameterTypes.size > 1 && parameterTypes.head.equals("none")) Option(parameterTypes.tail) // Removes the "none" parameter type, that is no longer needed.
        else Option(parameterTypes)
      })

    // Determines the maximal number of parameters.
    val maxNumberOfParameters: Int = if (parameterConfigurations.isEmpty) 1 else parameterConfigurations
      .map((parameterTypes: Seq[String]) => parameterTypes.size)
      .sorted(Ordering[Int].reverse).head

    // Determines the unconditional generic parameter type signature.
    val parameterSignatureString: String = parameterConfigurations.flatMap((parameterTypes: Seq[String]) => {
        (parameterTypes ++ Seq.fill(maxNumberOfParameters - parameterTypes.size)("none")).zipWithIndex
      }).groupBy((parameterTypes: String, index: Int) => index)
      .map((index: Int, parameterTypes: Seq[(String, Int)]) => {
        val parameterTypeList: Seq[String] = parameterTypes.map((parameterType: String, index: Int) => parameterType)
          .distinct.sorted
        if (parameterTypeList.size == 1 && !parameterTypeList.head.equals("none")) parameterTypeList.head else {
          // If the parameter type a the current parameter position is conditional.
          val parameterTypeNames: String = parameterTypeList.mkString(";")
          s"choice[$parameterTypeNames]"
        }
      }).mkString(",")

    // Create the parameter definition code.
    val parameterNodeCode: String = parameterNodes.map((parameter: Ast) => parameter.root.get match {
      case parameterNode: NewMethodParameterIn => parameterNode.code
      case conditionalNode: NewControlStructure => "\n" + conditionalNode.code + "\n"
    }).mkString(", ")

    // Returns the importen parts.
    (parameterNodes, parameterSignatureString, parameterNodeCode)
  }

  private def handleOneFunctionParameter(parameterIdentifierDeclarationNode: Node,
                                         converterState: VAstConverterState): Seq[Ast] = {

    // Selects the root parameter type node and the root parameter name node.
    val parameterTypeRootNode: Node = parameterIdentifierDeclarationNode.getNode(0)
    val parameterNameRootNode: Node = parameterIdentifierDeclarationNode.getNode(1)
    
    // Handles the parameter type.
    conditionalHandler.handleAndSimplifyConditionalExtended(parameterTypeRootNode, converterState, (parameterTypeNode: Node, parameterTypeState: VAstConverterState) => {
      variableHandler.handleVariableType(parameterTypeNode, parameterTypeState,
        (parameterNameRootState: VAstConverterState, parameterType: String, line: Option[Int], column: Option[Int]) => {
        handleParameterName(parameterNameRootNode, parameterNameRootState, parameterType, line, column,
                            parameterIdentifierDeclarationNode)
      })
    })
  }
  
  private def handleParameterName(parameterNameRootNode: Node, converterState: VAstConverterState, parameterType: String,
                                  line: Option[Int], column: Option[Int],
                                  parameterIdentifierDeclarationNode: Node): Seq[Ast] = {

    val parameterCreator: (Node, VAstConverterState, String, String, String) => Seq[Ast] =
      (nameNode: Node, nameNodeState: VAstConverterState, fullParameterType: String, parameterName: String, code: String) => {
        // The parameter index for each parameter Nod is set after all parameter nodes are translated and in the
        // right order because in some conditional situations the parameters in the SuperC AST may not in order.
        val parameterNode: NewMethodParameterIn = vAstCreator.parameterInNodeHelper(parameterIdentifierDeclarationNode, parameterName, code,
          -1, false, "BY_VALUE", fullParameterType, dynamicTypeHintFullName=Seq(), line=line, column=column)
        Seq(vAstCreator.AstHelper(parameterNode))
      }
    
    conditionalHandler.handleAndSimplifyConditionalExtended(parameterNameRootNode, converterState, (parameterNameNode: Node, parameterNameState: VAstConverterState) => {
      variableHandler.handleDeclaration(parameterNameNode, parameterType, parameterNameState, parameterCreator)
    })
  }

  private def combinedConditionSatisfiable(conditions: String, parameterCondition: String): Boolean = {
    val allConditions: Seq[String] = conditions.split(" ;; ").toSeq ++ Seq(parameterCondition)
    val combinedCondition: String = logicHandler.combineAndSimplifyConditionsAnd(allConditions)
    logicHandler.isSatisfiable(combinedCondition)
  }

  /**
   * Translates the methode instructions from the SuperC format into the JOERN format. This implementation simplifies
   * and combines conditional instruction sequences while preserving the code instruction order.
   *
   * **Important Notes:**
   * This implementation generates the code of the sub AST. The generated code does not necessarily match the actual
   * source code, it is only semantically identical.
   *
   * @param instructionSuperCRootNode The SuperC method instruction root node.
   * @param converterState The converter state that is passed to the `VAstPatternConverterForFunctionDeclarations`.
   * @return Returns the methode instruction JOERN AST with a code block node as root.
   */
  private def getMethodInstructionBlock(instructionSuperCRootNode: Node,
                                        converterState: VAstConverterState): (Ast, String) = {
    // Defines the Method instruction handler.
    val extractMethodeInstructions: (Node, VAstConverterState) => Seq[Ast] = (rootNode: Node, converterState: VAstConverterState) => {
      val methodeRootNode: Node = rootNode.getNode(1)
      val numberOfChildNodes: Int = methodeRootNode.size
      var methodInstructions: Seq[Ast] = Seq()

      for (nodeIndex: Int <- 0 until numberOfChildNodes) {
        methodInstructions = methodInstructions ++ converter.convert(methodeRootNode.getNode(nodeIndex), converterState)
      }
      methodInstructions
    }

    // Translates the method instructions of the current method.
    val conditionalHandler: VAstConditionalHandler = converter.getConditionalHandler
    val instructionAsts: Seq[Ast] = if (conditionalHandler.isSuperCConditionalNode(instructionSuperCRootNode)) {
      conditionalHandler.handleAndSimplifyConditional(instructionSuperCRootNode, converterState, extractMethodeInstructions)
    } else extractMethodeInstructions(instructionSuperCRootNode, converterState)

    // Ensures, that all instructions are combined into one AST with a code block node as root node.
    if (requireCodeBlock(instructionAsts)) {
      // If the method instruction Seq contains multiply ASTs or a ASTs without a code block node as root.

      // Determines the code position of the method instrcution code block.
      val (line: Option[Int], column: Option[Int]) = instructionAsts.map((instructionAst: Ast) => {
        if (instructionAst.root.isDefined) {
          val rootNode: AstNodeNew = instructionAst.root.get.asInstanceOf[AstNodeNew]
          (rootNode.lineNumber, rootNode.columnNumber)
        } else (None, None)
      }).sortBy((line: Option[Int], column: Option[Int]) => (line.getOrElse(Int.MaxValue), column.getOrElse(Int.MaxValue)))
        .headOption.getOrElse((None, None))

      // Sorts the instruction ASTs.
      val orderedInstructionAsts: Seq[Ast] = instructionAsts.sortBy((instructionAst: Ast) => instructionAst.root match {
        case Some(rootNode) =>
          val node: AstNodeNew = rootNode.asInstanceOf[AstNodeNew]
          (node.lineNumber.getOrElse(Int.MaxValue), node.columnNumber.getOrElse(Int.MaxValue))
        case None => (Int.MaxValue, Int.MaxValue)
      })

      val codeBlockNode: NewBlock = vAstCreator.emptyBlockNodeHelper(instructionSuperCRootNode, line, column)
      var codeBlockAst: Ast = vAstCreator.AstHelper(codeBlockNode)
      var codeSeq: Seq[String] = Seq.empty[String]
      for (instructionAst: Ast <- orderedInstructionAsts) {
        instructionAst.root match {
          case rootNode if rootNode.isDefined && (rootNode.get.nodeKind == JOERN_BLOCK_NODE_KIND)
            && rootNode.get.label.equals(JOERN_BLOCK_NODE_LABEL) =>
            // Adds the instructions contained in the code block AST to the method instruction code block.
            val blockNode: NewNode = rootNode.get
            codeSeq = codeSeq ++ Seq(rootNode.get.asInstanceOf[AstNodeNew].code)
            codeBlockAst = Ast(
              nodes = codeBlockAst.nodes ++ instructionAst.nodes.filterNot((node: NewNode) => node == blockNode),
              edges = codeBlockAst.edges ++ instructionAst.edges
                .map((edge: AstEdge) => if (edge.src == blockNode) AstEdge(codeBlockNode, edge.dst) else edge),
              conditionEdges = codeBlockAst.conditionEdges ++ instructionAst.conditionEdges
                .map((edge: AstEdge) => if (edge.src == blockNode) AstEdge(codeBlockNode, edge.dst) else edge),
              argEdges = codeBlockAst.argEdges ++ instructionAst.argEdges
                .map((edge: AstEdge) => if (edge.src == blockNode) AstEdge(codeBlockNode, edge.dst) else edge),
              receiverEdges = codeBlockAst.receiverEdges ++ instructionAst.receiverEdges
                .map((edge: AstEdge) => if (edge.src == blockNode) AstEdge(codeBlockNode, edge.dst) else edge),
              refEdges = codeBlockAst.refEdges ++ instructionAst.refEdges
                .map((edge: AstEdge) => if (edge.src == blockNode) AstEdge(codeBlockNode, edge.dst) else edge),
              bindsEdges = codeBlockAst.bindsEdges ++ instructionAst.bindsEdges
                .map((edge: AstEdge) => if (edge.src == blockNode) AstEdge(codeBlockNode, edge.dst) else edge),
              captureEdges = codeBlockAst.captureEdges ++ instructionAst.captureEdges
                .map((edge: AstEdge) => if (edge.src == blockNode) AstEdge(codeBlockNode, edge.dst) else edge)
            )

          case rootNode if rootNode.isDefined =>
            // Adds the instruction contained in the Asts to the method instruction code block.
            codeSeq = codeSeq ++ Seq(rootNode.get.asInstanceOf[AstNodeNew].code)
            codeBlockAst = codeBlockAst.withChild(instructionAst)

          case _ => // Ignores empty Asts.
        }
      }

      // Creates the new single root block node.
      val code: String = codeSeq.mkString("\n")
      val codeBlockCode: String = s"{\n$code\n}"
      codeBlockNode.code(codeBlockCode)
      (codeBlockAst, codeBlockCode)

    } else {
      // If the methode instruction Seq contains only one AST that has a code block as its root node.
      val rootAst: Ast = instructionAsts.head
      val code: String = rootAst.root match {
        case Some(node) => node.asInstanceOf[NewBlock].code
        case None => "<empty>"
      }
      (rootAst, code)
    }
  }

  /**
   * Check if a code block has to be created.
   *
   * @param methodeInstructionAsts All method instruction ASTs.
   * @return Returns `true` if a code block has to be created otherwise `false` is returned.
   */
  private def requireCodeBlock(methodeInstructionAsts: Seq[Ast]): Boolean = {
    if (methodeInstructionAsts.size != 1) {
      true
    } else {
      val astRootNode: Option[NewNode] = methodeInstructionAsts.head.root
      astRootNode.isEmpty
        || !((astRootNode.get.nodeKind == JOERN_BLOCK_NODE_KIND) && astRootNode.get.label.equals(JOERN_BLOCK_NODE_LABEL))
        || !converter.getConditionalHandler.isJoernChoiceNode(astRootNode.get)
    }
  }
}
