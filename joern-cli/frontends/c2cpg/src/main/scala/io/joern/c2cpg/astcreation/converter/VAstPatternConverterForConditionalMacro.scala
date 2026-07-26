package io.joern.c2cpg.astcreation.converter

import io.circe.syntax.*
import io.circe.parser.*
import io.joern.c2cpg.astcreation.VAstCreatorNew
import io.joern.x2cpg.{Ast, AstEdge}
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, nodes}
import io.shiftleft.codepropertygraph.generated.nodes.{AstNodeNew, NewBlock, NewControlStructure, NewNode}
import superc.core.PresenceConditionManager.PresenceCondition
import xtc.tree.{GNode, Node}

import scala.collection.JavaConverters.mapAsScalaMapConverter
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class VAstPatternConverterForConditionalMacro(vAstCreator: VAstCreatorNew, converter: VAstConverter)
  extends VAstPatternConverter(vAstCreator, converter, List.apply("Conditional")) with VAstConditionalHandler {

  private val SIMPLE_CONDITIONAL_MACRO: Int = 2
  private val FULL_CONDITIONAL_MACRO: Int = 4

  private val FIRST_CONDITION_INFORMATION: Int = 0
  private val FIRST_CONDITION_SUBTREE: Int = 1
  private val SECOND_CONDITION_INFORMATION: Int = 2
  private val SECOND_CONDITION_SUBTREE: Int = 3

  private val NO_CONDITION: String = "1"
  private val JOERN_CONTROL_STRUCTURE_NODE_KIND: Short = 11
  private val JOERN_BLOCK_NODE_KIND = 6

  private val LINE_NUMBER_PROPERTY: String = "LINE_NUMBER"
  private val COLUMN_NUMBER_PROPERTY: String = "COLUMN_NUMBER"
  private val CODE_PROPERTY: String = "CODE"

  override def getInitialState: Any = Seq.empty[String]

  override def convert(superCVAst: Node, converterState: VAstConverterState): Option[Seq[Ast]] = {
    val handler: (Node, VAstConverterState) => Seq[Ast] =
      (node: Node, state: VAstConverterState) => converter.convert(node, state)
    val asts: Seq[Ast] = handelConditional(superCVAst, converterState, handler)
    Option(asts)
  }

  private def getLocationInformation(node: NewNode): (Int, Int) = {
    val nodeProperties: Map[String, Any] = node.properties
    val line: Int = nodeProperties(LINE_NUMBER_PROPERTY).asInstanceOf[Int]
    val column: Int = nodeProperties(COLUMN_NUMBER_PROPERTY).asInstanceOf[Int]
    (line, column)
  }

  /**
   * Creates a new SuperC sub VAST with the passed Conditional node as root Node.
   * 
   * @param conditionalNode         The conditional root node that need to be transformed.
   * @param converterState          The  converter state that should be used for the transformation.
   * @param conditionSubtreeCreator The creator method that should be used for the transformation of th conditional
   *                                subtrees. This method is called for each SuperC VAST subtree of the consecutive
   *                                conditional node tree.
   * @return Returns the converted SuperC VAST subtree with the passed conditional node as root node.
   */
  def createConditionalSuperCSubtree(conditionalNode: Node, converterState: VAstConverterState,
                                     conditionSubtreeCreator: (Node, VAstConverterState) => Node): Node = {
    require(isConditionalNode(conditionalNode),
            s"It as a \"Conditional\" node expected, but a \"${conditionalNode.getName}\" node was passed")

    // Extents the passed conditionalSubtreeCreator(...) to also handel multiple consecutive conditional nodes.
    val conditionalHandler: (String, Node, VAstConverterState) => Node = (condition: String, node: Node, state: VAstConverterState) => {
      val conditions: Seq[String] = state.getState(this).asInstanceOf[Seq[String]] ++ Seq(condition)
      val newConverterState: VAstConverterState = state.updateState(this, conditions)
      if (isConditionalNode(node)) {
        createConditionalSuperCSubtree(node, newConverterState, conditionSubtreeCreator)
      } else {
        conditionSubtreeCreator(node, newConverterState)
      }
    }

    // Handels the first condition
    val newSuperCSubtree: Node = GNode.create("Conditional", conditionalNode.size)
    newSuperCSubtree.add(FIRST_CONDITION_INFORMATION, conditionalNode.get(FIRST_CONDITION_INFORMATION))
    val firstConditionalSubtree: Node = conditionalHandler(getFirstCondition(conditionalNode),
                                                           conditionalNode.getNode(FIRST_CONDITION_SUBTREE),
                                                           converterState)
    newSuperCSubtree.add(FIRST_CONDITION_SUBTREE, firstConditionalSubtree)

    if (conditionalNode.size == FULL_CONDITIONAL_MACRO) {
      // If it is an IF-ELSE conditional node.
      newSuperCSubtree.add(SECOND_CONDITION_INFORMATION, conditionalNode.get(SECOND_CONDITION_INFORMATION))
      val secondConditionalSubtree: Node = conditionalHandler(getSecondCondition(conditionalNode).get,
                                                              conditionalNode.getNode(SECOND_CONDITION_SUBTREE),
                                                              converterState)
      newSuperCSubtree.add(SECOND_CONDITION_SUBTREE, firstConditionalSubtree)
    }

    newSuperCSubtree
  }

  /**
   * Translates the passed SuperC conditional node into the JOERN representation or ignores the conditional node if is
   * not required.
   *
   * @param conditionalNode The conditional root node that need to be translated.
   * @param converterState The current converter state.
   * @param conditionSubtreeCreator The conditional handler that should be called for the translation of the ASTs
   *                                subtrees.
   * @return Returns the translated JOERN VAST subtree.
   */
  def handelConditional(conditionalNode: Node, converterState: VAstConverterState,
                        conditionSubtreeCreator: (Node,  VAstConverterState) => Seq[Ast]): Seq[Ast] = {
    require(isConditionalNode(conditionalNode),
            s"It as a \"Conditional\" node expected, but a \"${conditionalNode.getName}\" node was passed")

    // Extents the passed conditionalSubtrreeCreator to also multiple consecutive conditional nodes.
    val conditionalHandler: (String, Node, VAstConverterState) => Seq[Ast] = (condition: String, node: Node, state: VAstConverterState) => {
      val conditions: Seq[String] = state.getState(this).asInstanceOf[Seq[String]] ++ Seq(condition)
      val newConverterState: VAstConverterState = state.updateState(this, conditions)
      if (isConditionalNode(node)) {
        handelConditional(node, newConverterState, conditionSubtreeCreator)
      } else {
        conditionSubtreeCreator(node, newConverterState)
      }
    }

    // Extracts conditions and subtrees.
    var (firstCondition: String, firstConditionalSubtree: Node, secondCondition: String, secondConditionalSubtree) =
      extractConditionsAndSubtrees(conditionalNode)

    // Generates the ASTs of the first condition.
    val firstConditionalSubAsts: Seq[Ast] = conditionalHandler(firstCondition, firstConditionalSubtree, converterState)

    if (firstCondition == NO_CONDITION) {
      // If the condition of the conditional node is always true => ignore conditional node
      firstConditionalSubAsts

    } else {
      // If the conditional node contains a condition.

      // Generates the AST of the first condition.
      var firstConditionalSubAst: Ast = combineAsts(firstConditionalSubtree, firstConditionalSubAsts)

      // Extracts second condition.
      var secondConditionalSubAsts: Seq[Ast] = Seq.empty[Ast]
      var secondConditionalSubAst: Ast = vAstCreator.AstHelper()
      if (conditionalNode.size == FULL_CONDITIONAL_MACRO) {
        // If the conditional node contains two subtrees/conditions.
        secondConditionalSubAsts = conditionalHandler(secondCondition, secondConditionalSubtree, converterState)
        secondConditionalSubAst = combineAsts(secondConditionalSubtree, secondConditionalSubAsts)

        // Standardizes the AST subtree representation.
        if (firstConditionalSubAst.root.isEmpty) {
          // If the first condition contains an empty AST.
          firstConditionalSubAst = secondConditionalSubAst
          secondConditionalSubAst = vAstCreator.AstHelper()
          firstCondition = secondCondition
          secondCondition = ""
        }
      }

      // Checks if the conditional node is required to describe the conditional code.
      if (isNecessaryCondition(firstCondition, firstConditionalSubAst, secondCondition, secondConditionalSubAst)) {
        // If the current conditional Node is a required conditional node.
        Seq(createConditionalNode(conditionalNode, firstCondition, firstConditionalSubAst,
                                  secondCondition, secondConditionalSubAst))

      } else if (secondCondition.equals("")) {
        // If the current conditional node only replicated conditions.
        firstConditionalSubAsts

      } else {
        // If the current conditional node only replicated conditions.
        sortAstsByCodPosition(firstConditionalSubAsts ++ secondConditionalSubAsts)
      }
    }
  }

  private def combineAsts(rootNode: Node, asts: Seq[Ast]): Ast = {
    asts match {
      case astsSeq if astsSeq.isEmpty => vAstCreator.AstHelper()
      case astsSeq if astsSeq.size == 1 => astsSeq.head
      case astsSeq =>
        val astsOfInterested: Seq[Ast] = astsSeq.filter(ast => ast.nodes.nonEmpty)
        val firstBlockNode: NewNode = astsOfInterested.head.root.get

        val properties = firstBlockNode.propertiesMap
        val line: Option[Int] = if (properties.containsKey(LINE_NUMBER_PROPERTY)) {
          Option(properties.get(LINE_NUMBER_PROPERTY).asInstanceOf[Int])
        } else None
        val column: Option[Int] = if (properties.containsKey(COLUMN_NUMBER_PROPERTY)) {
          Option(properties.get(COLUMN_NUMBER_PROPERTY).asInstanceOf[Int])
        } else None
        val code: String = astsOfInterested.map(ast => ast.root.get.properties("CODE").asInstanceOf[String])
          .mkString("\n")

        val blockNode: NewBlock = vAstCreator.blockNodeHelper(rootNode, code, "<???>", line, column)
        vAstCreator.blockAstHelper(blockNode, astsOfInterested.toList)
    }
  }

  def handelAndSimplifyConditional(conditionalNode: Node, converterState: VAstConverterState,
                                   conditionSubtreesCreator: (Node, VAstConverterState) => Seq[Ast]): Seq[Ast] = {
    require(isConditionalNode(conditionalNode),
      s"It as a \"Conditional\" node expected, but a \"${conditionalNode.getName}\" node was passed")

    // Extents the passed conditionalSubtrreeCreator(...) to also handel multiple consecutive conditional nodes.
    val conditionalAstHandler: (String, Node, VAstConverterState) => Seq[Ast] = (condition: String, node: Node, state: VAstConverterState) => {
      val conditions: Seq[String] = state.getState(this).asInstanceOf[Seq[String]] ++ Seq(condition)
      val newConverterState: VAstConverterState = state.updateState(this, conditions)
      if (isConditionalNode(node)) {
        // Recursive handling of consecutive conditional node.
        handelAndSimplifyConditional(node, newConverterState, conditionSubtreesCreator)
      } else {
        // Creation and handling of sub ASTs.
        conditionSubtreesCreator(node, newConverterState).map(subAst => {
          val rootNode: Option[NewNode] = subAst.root
          if (rootNode.isDefined
              && (rootNode.get.nodeKind == JOERN_CONTROL_STRUCTURE_NODE_KIND)
              && !rootNode.get.asInstanceOf[NewControlStructure].presenceCondition.equals("<empty>")) {
            // if the root node of the sub AST is a conditional/chiose node.

            // Updates/extends the presence conditions of the conditional/choise node.
            val choiceNode: NewControlStructure = rootNode.get.asInstanceOf[NewControlStructure]
            val presenceCondition: Map[String, String] = getPresenceConditions(choiceNode)
            val newPresenceCondition = presenceCondition.view.mapValues((conditionString: String) => {
              combineAndSimplyConditions(conditions ++ Seq(conditionString))
            }).toMap
            choiceNode.presenceCondition = newPresenceCondition.asJson.noSpaces

            // Returns the updated sub AST.
            subAst

          } else {
            // If the root node of th sub AST is normale node.
            val conditionString: String = combineAndSimplyConditions(conditions)
            createConditionalNode(conditionalNode, conditionString, subAst)
          }
        })
      }
    }

    // Extracts conditions and subtrees.
    val (firstCondition: String, firstConditionalSubtree: Node, secondCondition: String, secondConditionalSubtree: Node) =
      extractConditionsAndSubtrees(conditionalNode)

    // Converts the SuperC subtrees.
    val firstConditionalSubAsts: Seq[Ast]  = conditionalAstHandler(firstCondition, firstConditionalSubtree, converterState)
    val secondConditionalSubAsts: Seq[Ast] = if (!secondCondition.equals("")) {
      conditionalAstHandler(secondCondition, secondConditionalSubtree, converterState)
    } else Seq.empty[Ast]

    val allSubAsts: Seq[Ast] = firstConditionalSubAsts ++ secondConditionalSubAsts
    val allSubAstsWithCondition: Seq[(String, Ast)] = allSubAsts.map((ast: Ast) => {
      val conditionalNode: NewControlStructure = ast.root.get.asInstanceOf[NewControlStructure]
      (getPresenceConditions(conditionalNode)("AST1"), ast)
    })
    val subAsts: Seq[Ast] = groupSubAsts(allSubAstsWithCondition).map((condition: String, ast: Ast) => {
      if (condition.equals("") || condition.equals(NO_CONDITION)) {
        // If the condition is a tautology.
        // Creates the unconditional conditional AST.
        val rootNode: NewNode = ast.root.get
        Ast(
          nodes = ast.nodes.filterNot((node: NewNode) => node == rootNode),
          edges = ast.edges.filterNot((edge: AstEdge) => (edge.src == rootNode) || (edge.dst == rootNode)),
          conditionEdges = ast.conditionEdges,
          argEdges = ast.argEdges,
          receiverEdges = ast.receiverEdges,
          refEdges = ast.refEdges,
          bindsEdges = ast.bindsEdges,
          captureEdges = ast.captureEdges
        )

      } else {
        // If only the condition has to be updated.
        // Updates the condition.
        val conditionalNode: NewControlStructure = ast.root.get.asInstanceOf[NewControlStructure]
        updatePresenceCondition(conditionalNode, Map("AST1" -> condition))

        // Updates the Condition in the code
        val subtreeCode: String = ast.edges.filter((edge: AstEdge) => edge.src == conditionalNode).head.dst
                                     .properties(CODE_PROPERTY).asInstanceOf[String]
        conditionalNode.code(s"#IF $condition:\n$subtreeCode\n#ENDIF")

        // If the JOERN choice nodes also contain a code position, this position needs to be updated in some
        // situration/implementations.
        ast
      }
    })
    sortAstsByCodPosition(subAsts)
  }

  /**
   * Sorts the AST sequence by the first code position of each AST.
   *
   * @param asts The ASTs that should be sorted by their first code position.
   * @return Returns the sorted ASTs as sequence.
   */
  private def sortAstsByCodPosition(asts: Seq[Ast]): Seq[Ast] = asts.sortWith((ast1: Ast, ast2: Ast) => {
    //
    val position1: Option[(Int, Int)] = getFirstAstCodePosition(ast1)
    val position2: Option[(Int, Int)] = getFirstAstCodePosition(ast2)
    val (line1: Int, column1: Int) = if (position1.isDefined) position1.get else (Int.MaxValue, Int.MaxValue)
    val (line2: Int, column2: Int) = if (position2.isDefined) position2.get else (Int.MaxValue, Int.MaxValue)

    (line1 < line2) || ((line1 == line2) && (column1 < column2))
  })

  /**
   * Returns the earliest code position that appears in the given AST and does not belong to a choice/conditional node.
   * If no such position exists, None is returned.
   *
   * **Important:**
   * The code position of the conditional/choice node is not considered because, at the moment, conditional/choice nodes
   * do not contain code position information.
   *
   * @param ast The AST that first code positon should be determined.
   * @return Returns the earliest code position of the AST that does not belong to a choice/conditional node or `None`
   *         if no such position exist.
   */
  private def getFirstAstCodePosition(ast: Ast): Option[(Int, Int)] = {
    if (ast.root.isEmpty) {
      // If the passed  AST does not contain of at least one node.
      None

    } else {
      // Get First non-conditional Node Position.
      var firstLine: Int = Int.MaxValue
      var firstColumn: Int = Int.MaxValue
      val pendingNodes: mutable.Queue[NewNode] = mutable.Queue(ast.root.get)
      while (pendingNodes.nonEmpty) {
        val currentNode: NewNode = pendingNodes.dequeue()
        if (isJoernChoiceNode(currentNode)) {
          val childNodes: Seq[NewNode] = ast.edges.filter((edge: AstEdge) => edge.src.equals(currentNode))
                                            .map((edge: AstEdge) => edge.dst).toSeq
          pendingNodes.enqueueAll(childNodes)
        } else {
          val line: Option[Int] = currentNode.properties.get(LINE_NUMBER_PROPERTY).asInstanceOf[Option[Int]]
          val column: Option[Int] = currentNode.properties.get(COLUMN_NUMBER_PROPERTY).asInstanceOf[Option[Int]]
          if (line.isDefined && (firstLine > line.get || (firstLine == line.get && column.isDefined && firstColumn > column.get))) {
            firstLine = line.get
            firstColumn = column.get
          }
        }
      }

      if (firstLine == Int.MaxValue) {
        // If the AST only contains nodes without positions.
        None

      } else {
        // Returns the first code position of the passed AST.
        Option(firstLine, firstColumn)
      }
    }
  }

  /**
   * Checks if the passed new JOERN node is a conditional/choise node.
   *
   * @param node The new JOERN node that should be checked for whether it is a conditioal/choice node is.
   * @return Returns `true` if the passed new JOERN node is a conditional/choice node, otherwise `false` is returned.
   */
  private def isJoernChoiceNode(node: NewNode): Boolean = (node.nodeKind == JOERN_CONTROL_STRUCTURE_NODE_KIND)
    && node.asInstanceOf[NewControlStructure].controlStructureType == ControlStructureTypes.CHOICE

  private def combineAndSimplyTwoConditions(firstConditions: String, secondCondition: String,
                                            fullSimplification: Boolean = false): String = {
    // Combines the two conditions in disjunctive form.
    val firstConditionsParts: Seq[String] = firstConditions.split(" \\|\\| ")
    val secondConditionParts: Seq[String] = secondCondition.split(" \\|\\| ")
    val combinedTerm: Seq[Seq[String]] = firstConditionsParts.flatMap((firstPart: String) => {
      val firstPartSubParts: Seq[String] = firstPart.split( " && ")
      secondConditionParts.map((secondPart: String) => {
        (firstPartSubParts ++ secondPart.split(" && ")).distinct.sorted
      })
    })

    // Simplifies the created new expression.
    var simplifiedCombinedTerm: Seq[Seq[String]] = simplify(combinedTerm)
    if (fullSimplification) {
      simplifiedCombinedTerm = getPrimeImplicants(simplifiedCombinedTerm)
    }

    simplifiedCombinedTerm.map((innerPart: Seq[String]) => innerPart.mkString(" && ")).mkString(" || ")
  }

  private def combineAndSimplyConditions(conditions: Seq[String]): String = {
    conditions.size match {
      case 0 => ""
      case 1 => conditions.head
      case _ =>
        var combinedConditions: String = conditions.head
        for (condition <- conditions.tail) {
          combinedConditions = combineAndSimplyTwoConditions(combinedConditions, condition, condition == conditions.last)
        }
        val finalC: String = combinedConditions.split(" \\|\\| ").distinct.mkString(" || ")
        val initial: String = conditions.mkString(") && (")
        finalC
    }
  }

  private def getPresenceConditions(conditionalNode: NewControlStructure): Map[String, String] = {
    decode[Map[String, String]](conditionalNode.presenceCondition) match {
      case Right(map) => map
      case Left(error) =>
        require(1 == 0, s"Failed to parse: $error")
        Map()
    }
  }

  private def updatePresenceCondition(conditionalNode: NewControlStructure,
                                      presenceCondition: Map[String, String]): Unit = {
    conditionalNode.presenceCondition = presenceCondition.asJson.noSpaces
  }

  private def extractConditionsAndSubtrees(conditionalNode: Node): (String, Node, String, Node) = {
    require(isConditionalNode(conditionalNode),
      s"It as a \"Conditional\" node expected, but a \"${conditionalNode.getName}\" node was passed")

    // Extracts the first condition and its AST.
    val firstCondition: String = getFirstCondition(conditionalNode)
    val firstConditionalSubtree: Node = getFirstConditionalSubtree(conditionalNode)

    // Extracts the second condition and its AST if defined.
    val secondCondition: String = if (conditionalNode.size == FULL_CONDITIONAL_MACRO) {
      getSecondCondition(conditionalNode).get
    } else ""
    val secondConditionalSubtree: Node = if (conditionalNode.size == FULL_CONDITIONAL_MACRO) {
      getSecondConditionalSubtree(conditionalNode).get
    }  else  null

    (firstCondition, firstConditionalSubtree, secondCondition, secondConditionalSubtree)
  }

  /**
   * Checks if a conditional Node is required.
   *
   * @param firstCondition
   * @param firstConditionalSubAst
   * @param secondCondition
   * @param secondConditionalSubAst
   * @return
   */
  private def isNecessaryCondition(firstCondition: String, firstConditionalSubAst: Ast,
                                   secondCondition: String, secondConditionalSubAst: Ast): Boolean = {
    // Checks if the conditional node is required to describe the conditional code.
    var isNecessaryConditionalNode: Boolean = true
    if (firstConditionalSubAst.root.isEmpty && secondConditionalSubAst.root.isEmpty) {
      // If both sub ASTs ar empty.
      isNecessaryConditionalNode = false

    } else if (firstCondition.equals(NO_CONDITION)
                && ((secondCondition == null) || secondCondition.equals("") || secondCondition.equals(NO_CONDITION))) {
      // If the defined conditions are always satisfied.
      isNecessaryConditionalNode = false

    /**} else if (secondConditionalSubAst.root.isEmpty) {
      // If the conditional node only contains one subtree/condition.

      // Checks if the current conditional Node is necessary or an unnecessary duplicate.
      if (firstConditionalSubAst.root.get.nodeKind == JOERN_CONTROL_STRUCTURE_NODE_KIND) {
        val conditionalNode: NewControlStructure = firstConditionalSubAst.root.get.asInstanceOf[NewControlStructure]
        if (conditionalNode.controlStructureType == ControlStructureTypes.CHOICE) {
          val conditions: Map[String, String] = getPresenceConditions(conditionalNode)
          val conditionsStrings: Seq[String] = conditions.toSeq
            .filter((key, value) => key.equals("AST1") || key.equals("AST2"))
            .map((key, value) => value)

          // Extracts all interesting conditional options.
          val conditionOptions: Seq[String] = if (conditionsStrings.size == 1) {
            Seq(conditionsStrings.head)
          } else {
            // TODO: Generalize the pattern matching by simplifying the conditions when combining the two subconditions.
            //  To do this, use the `combainAndSimplify(...)` method.
            Seq(s"${conditionsStrings.head}||${conditionsStrings.last}",
              s"${conditionsStrings.head} || ${conditionsStrings.last}",
              s"${conditionsStrings.last}||${conditionsStrings.head}",
              s"${conditionsStrings.last} || ${conditionsStrings.head}")
          }
          isNecessaryConditionalNode = (!conditionOptions.exists(condition => condition.equals(firstCondition)))
            || ((conditionOptions.size == 1) && conditionOptions.head.contains(firstCondition))
        }
      }**/

    } else {
      // Checks if both child sub ASTs consists of on block that containes only conditional/choice node.
      var preliminaryRequirementDecision: Boolean  = true

      // Checks the first child AST if the conditional node is requiert.
      val firstAstRootNode: NewNode = firstConditionalSubAst.root.get
      if (isJoernChoiceNode(firstAstRootNode)) {
        // If the root node of the child AST is a conditional node.
        preliminaryRequirementDecision = false

      } else if (firstAstRootNode.nodeKind == JOERN_BLOCK_NODE_KIND) {
        // If the root node of the child AST is a block node.
        preliminaryRequirementDecision = !firstConditionalSubAst.edges.forall((edge: AstEdge) => {
          (edge.src != firstAstRootNode) || ((edge.dst.nodeKind == JOERN_CONTROL_STRUCTURE_NODE_KIND)
            && edge.dst.asInstanceOf[NewControlStructure].controlStructureType.equals(ControlStructureTypes.CHOICE))
        })
      }

      println(s"first condition: \"$firstCondition\" des.: $preliminaryRequirementDecision")
      
      if (!preliminaryRequirementDecision) {
        if ((secondConditionalSubAst == null) || secondConditionalSubAst.root.isEmpty) {
          isNecessaryConditionalNode = false
        } else {
          val secondAstRootNode: NewNode = secondConditionalSubAst.root.get
          if (isJoernChoiceNode(secondAstRootNode)) {
            // If the root node of the child AST is a conditional node.
            isNecessaryConditionalNode = false

          } else if (secondAstRootNode.nodeKind == JOERN_BLOCK_NODE_KIND) {
            // If the root node of the child AST is a block node.
            isNecessaryConditionalNode = !secondConditionalSubAst.edges.forall((edge: AstEdge) => {
              (edge.src != secondAstRootNode) || ((edge.dst.nodeKind == JOERN_CONTROL_STRUCTURE_NODE_KIND)
                && edge.dst.asInstanceOf[NewControlStructure].controlStructureType.equals(ControlStructureTypes.CHOICE))
            })
          }
        }
      }
    }
    isNecessaryConditionalNode
  }

  /**
   * Creates an AST where the root node is the conditional/choice node to be generated, and returns it.
   *
   * **Important Notes:**
   * 1. This implementation generates the code of the sub AST. The generated code does not necessarily match the actual
   *    source code, it is only semantically identical.
   * 2. This implementation does not store the code position of the conditional/preprocessor instructions in the
   *    generated choice node because, as at the current view, this information is not needed for a code analyses with
   *    JoERN. If the code position is later required, it can be determined manually using the AST and the source code.
   *    However, if the code positions are to be stored in the choice node at a later stage, they have to be
   *    reconstructed using the source coder stored in the `VAstConverterState`. To do this, all positions of `#IFDEF`,
   *    `#IFNDEF`, `#IF`, `#ELSE`, and `#ELIF` have to first be determined and annotated with the corresponding
   *    Disjunctive Minimal Forms (DMFs) (to determine all logical expressions, the positions of the `#ENDIF` statements
   *    are also necessary, but these do not need to be permanently stored). It is recommended to store the generated
   *    mapping from DMF to position as a map in addition to the logic expression stack in `VAstConverterState` and to
   *    compute it once during the initial creation of the choice node for the entire source code.
   *
   * @param conditionalNode The SuperC conditional node
   * @param firstCondition
   * @param firstConditionSubtree
   * @param secondCondition
   * @param secondConditionSubtree
   * @return Returns the created AST.
   */
  private def createConditionalNode(conditionalNode: Node, firstCondition: String, firstConditionSubtree: Ast,
                                    secondCondition: String = "", secondConditionSubtree: Ast = vAstCreator.AstHelper()): Ast = {
    // Cheks the requirements.
    require((firstCondition != null) && (firstConditionSubtree != null) && firstConditionSubtree.root.isDefined,
            "A conditional Node can only be created if at least the firest subtree and condition is defined")

    require((secondCondition.equals("") && ((secondConditionSubtree == null) || secondConditionSubtree.root.isEmpty))
            || ((!secondCondition.equals("")) && (secondConditionSubtree != null) && secondConditionSubtree.root.isDefined),
            "If a conditional node with two conditions is desired, both the second condition and a second AST hast to be passed.")

    val firstCodePart: String = firstConditionSubtree.root.get.asInstanceOf[AstNodeNew].code
    val code: String = if ((secondConditionSubtree == null) || secondConditionSubtree.root.isEmpty) {
      // If the conditional node contains one subtree/condition.
      s"#IF $firstCondition:\n$firstCodePart\n#ENDIF" // TODO: In some cases the "#IF" has to be replaced by "#IFDEF".

    } else {
      // If the conditional node contains two subtrees/conditions.
      val secondCodePart: String = secondConditionSubtree.root.get.asInstanceOf[AstNodeNew].code
      s"#IF $firstCondition:\n$firstCodePart\n#ELIF $secondCondition\n$secondCodePart\n#ENDIF" // TODO: In some cases the "#IF" has to be replaced by "#IFDEF".
    }

    // Defines the presence conditions
    val presenceConditionMap: Map[String, String] = if (secondCondition.equals("")) {
      Map("AST1" -> firstCondition)
    } else {
      Map("AST1" -> firstCondition, "AST2" -> secondCondition)
    }

    // Creates the conditional node.
    val choiceNode: NewControlStructure =
      vAstCreator.controlStructureNodeHelper(conditionalNode, ControlStructureTypes.CHOICE, code)

    // Creates the conditional nodes.
    var presenceConditionEdges: Seq[AstEdge] = Seq(AstEdge(choiceNode, firstConditionSubtree.root.get))
    if ((secondConditionSubtree != null) && (secondConditionSubtree.root.isDefined)) {
      presenceConditionEdges = presenceConditionEdges :+ AstEdge(choiceNode, secondConditionSubtree.root.get)
    }

    // Adds the presence conditions
    val presenceConditionMapSerialized = presenceConditionMap.asJson.noSpaces
    choiceNode.presenceCondition(presenceConditionMapSerialized)

    // Creates the conditional AST.
    Ast(
      nodes = Seq(choiceNode) ++ firstConditionSubtree.nodes ++ secondConditionSubtree.nodes,
      edges = firstConditionSubtree.edges ++ secondConditionSubtree.edges ++ presenceConditionEdges,
      conditionEdges = firstConditionSubtree.conditionEdges ++ secondConditionSubtree.conditionEdges, //++ Seq(AstEdge(choiceNode, choiceNode)), //TODO: ++ presenceConditionEdges?
      argEdges = firstConditionSubtree.argEdges ++ secondConditionSubtree.argEdges,
      receiverEdges = firstConditionSubtree.receiverEdges ++ secondConditionSubtree.receiverEdges,
      refEdges = firstConditionSubtree.refEdges ++ secondConditionSubtree.refEdges,
      bindsEdges = firstConditionSubtree.bindsEdges ++ secondConditionSubtree.bindsEdges,
      captureEdges = firstConditionSubtree.captureEdges ++ secondConditionSubtree.captureEdges
    )
  }

  def isConditionalNode(node: Node): Boolean = node.isInstanceOf[GNode] &&node.getName.equals("Conditional")

  def getFirstCondition(node: Node): String = {
    require(isConditionalNode(node), "A conditional node was expected, but a node of a different node type was passed.")
    node.get(FIRST_CONDITION_INFORMATION).asInstanceOf[PresenceCondition].toString
  }

  def getSecondCondition(node: Node): Option[String] = {
    require(isConditionalNode(node), "A conditional node was expected, but a node of a different node type was passed.")
    if (node.size == FULL_CONDITIONAL_MACRO) {
      Option(node.get(SECOND_CONDITION_INFORMATION).asInstanceOf[PresenceCondition].toString)
    } else {
      None
    }
  }

  def getFirstConditionalSubtree(node: Node): Node = {
    require(isConditionalNode(node), "A conditional node was expected, but a node of a different node type was passed.")
    node.getNode(FIRST_CONDITION_SUBTREE)
  }

  def getSecondConditionalSubtree(node: Node): Option[Node] = {
    require(isConditionalNode(node), "A conditional node was expected, but a node of a different node type was passed.")
    if (node.size == FULL_CONDITIONAL_MACRO) {
      Option(node.getNode(SECOND_CONDITION_SUBTREE))
    } else {
      None
    }
  }

  def getAllConditionalAstSubtrees(node: Node): Seq[Node] = {
    if (isConditionalNode(node)) {
      if (node.size == SIMPLE_CONDITIONAL_MACRO) {
        getAllConditionalAstSubtrees(node.getNode(FIRST_CONDITION_SUBTREE))
      } else {
        getAllConditionalAstSubtrees(node.getNode(FIRST_CONDITION_SUBTREE))
          ++ getAllConditionalAstSubtrees(node.getNode(SECOND_CONDITION_SUBTREE))
      }
    } else {
      Seq(node)
    }
  }


  private def simplify(exp: Seq[Seq[String]]): Seq[Seq[String]] = {
    var terms: Set[Set[String]] = exp.flatMap(normalizeTerm)
      .map((term: Seq[String]) => term.toSet.filterNot((variable: String) => variable.equals("1")))
      .toSet

    var changed = true
    while (changed) {
      val before = terms

      // Remove terms that are covered by shorter terms.
      terms = removeAbsorbedTerms(terms)

      // Combine terms, e.g.: (a && b) || (a && !b)  =>  (a)
      val combinedTerms = for {
        a <- terms
        b <- terms
        combined <- combine(a, b)
      } yield combined

      // (a && b) || (a) => (a)
      terms = removeAbsorbedTerms(terms ++ combinedTerms)
      changed = terms != before
    }

    terms.toList.sortBy(term => (term.size, term.toList.sorted.mkString(","))).map(_.toList.sorted)
  }

  private def normalizeTerm(term: Seq[String]): Option[Seq[String]] = {
    val literals: Seq[String] = term.distinct
    val variable_names: Seq[String] = literals.map(removeNegation)

    // Checks whether the expression is satisfiable.
    if (variable_names.exists((name: String) => literals.contains(name) && literals.contains("!" + name))) None
    else Some(literals)
  }

  private def removeAbsorbedTerms(terms: Set[Set[String]]): Set[Set[String]] = {
    terms.filterNot((term: Set[String]) => terms.exists((other: Set[String]) => (other != term) && other.subsetOf(term)))
  }

  private def combine(a: Set[String], b: Set[String]): Option[Set[String]] = {
    val onlyA = a -- b
    val onlyB = b -- a

    if (onlyA.size == 1 && onlyB.size == 1) {
      val litA = onlyA.head
      val litB = onlyB.head

      if (negate(litA) == litB) Some(a intersect b) else None
    } else None
  }

  private def negate(literal: String): String = if (literal.startsWith("!")) literal.drop(1) else "!" + literal

  private def removeNegation(literal: String): String = if (literal.startsWith("!")) literal.drop(1) else literal

  private def getPrimeImplicants(dfTerms: Seq[Seq[String]]): Seq[Seq[String]] = {
    if (dfTerms.size <= 1) {
      // No simplification is possible
      dfTerms
      
    } else {
      // Simplify the expression with the Quine McCluskey algorithm.

      // Creates the map to convert the variable names to indices.
      val variableNames: List[String] = dfTerms.flatMap((terms: Seq[String]) => terms.map((variable: String) => removeNegation(variable))).toList.distinct
      val variableNameMap: Map[String, Int] = variableNames.zipWithIndex.toMap
      val numberOfVariableNames: Int = variableNames.size
  
      // Converts the DF termes to logic strings made of '0', '1' and '-' (0 and 1 at the same time)
      val allTrue: String = "-" * numberOfVariableNames
      val logicStrings: List[String] = dfTerms.map((term: Seq[String]) => {
        var logicString: String = allTrue
        for (variable <- term) {
          val variableName = removeNegation(variable)
          logicString = logicString.updated(variableNameMap(variableName), if (variable.startsWith("!")) '0' else '1')
        }
        logicString
      }).toList

      // Groups the logic string base on the number of contained '1' and wildcards ('-').
      val logicStringsGroupedByWildcards: mutable.Map[Int, mutable.Map[Int, mutable.Map[String, Set[Int]]]] = mutable.Map.from(logicStrings
        .zipWithIndex                                                           // List[(String, Int)]
        .map((logicString, refIndices) => (logicString, Set(refIndices)))       // List[(String, Set[Int])]
        .groupBy((logicString, refIndices) => logicString.count(_ == '-'))      // Map[Int, List[(String, Set[Int])]]
        .map((wildcards: Int, logicStringGroup: List[(String, Set[Int])]) => {  // Map[Int, Map[Int, Map[String, Set[Int]]]]
          val logicStringGroupByNumberOfOnes: mutable.Map[Int, mutable.Map[String, Set[Int]]] = mutable.Map.from(logicStringGroup
            .groupBy((logicString: String, refIndices: Set[Int]) => logicString.count(_ == '1'))  // Map[Int, List[(String, Set[Int]]
            .map((zeros: Int, expression: List[(String, Set[Int])]) => (zeros, mutable.Map.from(expression))))
          (wildcards, logicStringGroupByNumberOfOnes)
        }))

      // Calculates the prime implicants.
      var wildcards: Int = 0
      val nonPrimeImplicantsLogicStrings: mutable.Set[String] = mutable.Set.empty[String]
      val primeImplicantsLogicStrings: mutable.Map[String, Set[Int]] = mutable.Map.empty[String, Set[Int]]
      //while (logicStringsGroupedByWildcards.nonEmpty && logicStringsGroupedByWildcards.size > wildcards) {
      for (wildcardIndex: Int <- 0 until numberOfVariableNames) {
        wildcards = wildcardIndex
        val currentLogicStringsGroupedByOnes: mutable.Map[Int, mutable.Map[String, Set[Int]]] = logicStringsGroupedByWildcards.getOrElse(wildcards, mutable.Map.empty[Int, mutable.Map[String, Set[Int]]])
        wildcards += 1

        currentLogicStringsGroupedByOnes.size match {
          case 0 =>
            // No logic expressions defined => nothing to do
          case 1 =>
            // All logic strings are prime implicants.
            val keys: String = currentLogicStringsGroupedByOnes.mkString(", ")
            currentLogicStringsGroupedByOnes(currentLogicStringsGroupedByOnes.keys.head).foreach((logicString: String, refIndices: Set[Int]) => {
              Console.flush()
              if (primeImplicantsLogicStrings.contains(logicString)) {
                primeImplicantsLogicStrings(logicString) ++= refIndices
              } else {
                primeImplicantsLogicStrings.addOne((logicString, refIndices))
              }
            })
          case _ =>
            val nextLogicStringsGroupedByOnes: mutable.Map[Int, mutable.Map[String, Set[Int]]] = if (logicStringsGroupedByWildcards.contains(wildcards)) {
              logicStringsGroupedByWildcards(wildcards)
            } else {
              mutable.Map.empty[Int, mutable.Map[String, Set[Int]]]
            }
            val lowString: String = currentLogicStringsGroupedByOnes.mkString(", ")
            val upString: String = currentLogicStringsGroupedByOnes.mkString(", ")

            // The logic strings have to be compared pairwise inorder to simplify the logic expression.
            val allOneGroupIndices: Seq[Int] = currentLogicStringsGroupedByOnes.keys.toSeq.sorted
            for (lowerOneGroupIndex: Int <- allOneGroupIndices) {
              if (allOneGroupIndices.contains(lowerOneGroupIndex + 1)) {
                // If the comparison for two one logic string groups is necessary
                // Prepares everything for the simplification of the one logic string group.
                var lowerLogicStringGroup: mutable.Map[String, Set[Int]] = currentLogicStringsGroupedByOnes(lowerOneGroupIndex)
                val upperLogicStringGroup: mutable.Map[String, Set[Int]] = currentLogicStringsGroupedByOnes(lowerOneGroupIndex + 1)

                // Compare logic strings pairwise inorder to simplify the logic expression and determine the prime implicants.
                val newSimplifiesLogicStrings = upperLogicStringGroup.map((logicString: String, refIndices: Set[Int]) => {
                  // Checks all possible logic string simplifications for the current logic string
                  for (variableNameIndex <- 0 to numberOfVariableNames) {
                    if (logicString.charAt(variableNameIndex) == '0') {

                      // Checks if the twin logic string exist with a one at the position variableNameIndex and retrieves the ref indices.
                      val twinLogicString: String = logicString.patch(variableNameIndex, "1", 1)
                      val otherRefIndices: Set[Int] = upperLogicStringGroup
                        .filter((otherLogString: String, _: Set[Int]) => twinLogicString.equals(otherLogString))
                        .flatMap((_: String, otherRefIndices: Set[Int]) => otherRefIndices).toSet

                      if (otherRefIndices.nonEmpty) {
                        // If the twin logic string exist.
                        // Saves the new simplified logic string.
                        val newLogString: String = logicString.patch(variableNameIndex, "-", 1)
                        val newRefIndices: Set[Int] = refIndices ++ otherRefIndices
                        if (nextLogicStringsGroupedByOnes.contains(lowerOneGroupIndex)) {
                          val currentLogicStringsGroupedByOne: mutable.Map[String, Set[Int]] = nextLogicStringsGroupedByOnes(lowerOneGroupIndex)
                          val currentRefIndices: Set[Int] = currentLogicStringsGroupedByOne.getOrElse(newLogString, Set.empty[Int])
                          currentLogicStringsGroupedByOne(newLogString) = currentRefIndices ++ newRefIndices
                        } else {
                          nextLogicStringsGroupedByOnes.addOne((lowerOneGroupIndex, mutable.Map(newLogString -> newRefIndices)))
                        }

                        // Marks the logic string pair as non-prime implicants.
                        nonPrimeImplicantsLogicStrings.add(logicString)
                        nonPrimeImplicantsLogicStrings.add(twinLogicString)
                      }
                    }
                  }

                  // Adds the lower logic string to the prime implicants if it was not used in a reduction.
                  if (!nonPrimeImplicantsLogicStrings.contains(logicString)) {
                    primeImplicantsLogicStrings.addOne((logicString, refIndices))
                  }
                })

              } else {
                // If no further simplification comparisons for the current one logic string groups are required.
                // Adds the logic strings to the prime implicants if they were not used in a reduction.
                currentLogicStringsGroupedByOnes(lowerOneGroupIndex)
                  .filterNot((logicString: String, _: Set[Int]) => nonPrimeImplicantsLogicStrings.contains(logicString))
                  .foreach((logicExpression: (String, Set[Int])) => primeImplicantsLogicStrings.addOne(logicExpression))
              }
            }
            if (nextLogicStringsGroupedByOnes.nonEmpty) {
              logicStringsGroupedByWildcards.addOne((wildcards, nextLogicStringsGroupedByOnes))
            }
        }
      }

      // Finds the minimal prime implicant combination.
      val minimalPrimeImplicants: Seq[String] = primeImplicantsLogicStrings.toSeq
        .filterNot((logicString: String, refIndices: Set[Int]) => {
          primeImplicantsLogicStrings.exists((otherLogicString: String, otherRefIndices: Set[Int]) =>
            (logicString != otherLogicString) && refIndices.subsetOf(otherRefIndices))
        }).map((logicString: String, refIndices: Set[Int]) => logicString)

      // Reconstruct logical expression from the minimal prime implicant logic strings.
      val dmf: Set[Set[String]] = minimalPrimeImplicants.map((logicString: String) => {
        logicString.toList.zipWithIndex.filterNot((alg: Char, variableNameIndex: Int) => alg == '-')
          .map((alg: Char, variableNameIndex: Int) => {
            val variableName: String = variableNames(variableNameIndex)
            if ((alg == '0') == variableName.startsWith("!")) variableName else negate(variableName)
          }).toSet
      }).toSet

      dmf.map((part: Set[String]) => part.toSeq).toSeq
    }
  }

  private def groupSubAsts(asts: Seq[(String, Ast)]): Seq[(String, Ast)] = {
    if (asts.size <= 1) {
      asts
    } else {

      val groupedSubAsts: ListBuffer[(ListBuffer[String], Ast)] = ListBuffer((ListBuffer(asts.head(0)), asts.head(1)))
      for ((logicString: String, ast: Ast) <- asts.tail) {
        var notAssigned: Boolean = true
        for (index <- groupedSubAsts.indices) {
          if (notAssigned && sameAst(ast, groupedSubAsts(index)(1))) {
            groupedSubAsts(index)(0) += logicString
            notAssigned = false
          }
        }
        if (notAssigned) {
          groupedSubAsts.addOne((ListBuffer(logicString), ast))
        }
      }

      // Simplify conditional logic expression of the grouped Subtrees with.
      groupedSubAsts.map((logicString: ListBuffer[String], ast: Ast) => {
        val combinedTerm: Seq[Seq[String]] = logicString.mkString(" || ").split(" \\|\\| ").map((terms: String) => terms.split(" && ").toSeq).toSeq

        // Simplifies the combined expression.
        val normalizedCombinedTerm: Seq[Seq[String]] = simplify(combinedTerm)
        val simplifiedCombinedTerm: Seq[Seq[String]] = getPrimeImplicants(normalizedCombinedTerm)
        val simplifiedCombinedExpression: String = simplifiedCombinedTerm.map((innerPart: Seq[String]) => innerPart.mkString(" && ")).mkString(" || ")

        (simplifiedCombinedExpression, ast)
      }).toSeq
    }
  }

  private def sameAst(ast1: Ast, ast2: Ast): Boolean = {
    val conditionalNode1: NewNode = ast1.root.get
    val conditionalNode2: NewNode = ast2.root.get
    val root1: NewNode = ast1.edges.filter((edge: AstEdge) => edge.src.equals(conditionalNode1)).head.dst
    val root2: NewNode = ast2.edges.filter((edge: AstEdge) => edge.src.equals(conditionalNode2)).head.dst

    var seemsEquals: Boolean = true
    val pendingNodes: mutable.Stack[(NewNode, NewNode)] = mutable.Stack((root1, root2))
    while (seemsEquals && pendingNodes.nonEmpty) {
      val (node1: NewNode, node2: NewNode) = pendingNodes.pop

      // Compares the node kind, node object type and node label.
      if (node1.nodeKind != node2.nodeKind || node1.label != node2.label
          || !node1.getClass.toString.equals(node2.getClass.toString)) {
        seemsEquals = false
      } else {

        val nodeProperties1: Map[String, Any] = node1.propertiesMap.asScala.toMap
        val nodeProperties2: Map[String, Any] = node2.propertiesMap.asScala.toMap

        val nodePropertyKeys1 = nodeProperties1.keySet
        val nodePropertyKeys2 = nodeProperties1.keySet

        val prop1: String = nodePropertyKeys1.mkString(", ")
        val prop2: String = nodePropertyKeys1.mkString(", ")

        // Checks if the two keys are different.
        if (!nodePropertyKeys1.subsetOf(nodePropertyKeys2) || !nodePropertyKeys2.subsetOf(nodePropertyKeys1)) {
          seemsEquals = false
        } else {
          // Compares all node properties.
          seemsEquals = nodeProperties1.forall((key: String, value: Any) => value.equals(nodeProperties2(key)))

          // Selects all child nodes of th current two nodes.
          val childNodes1: Seq[NewNode] = ast1.edges.filter((edge: AstEdge) => edge.src.equals(node1)).map((edge: AstEdge) => edge.dst).toSeq
          val childNodes2: Seq[NewNode] = ast2.edges.filter((edge: AstEdge) => edge.src.equals(node2)).map((edge: AstEdge) => edge.dst).toSeq

          if (childNodes1.size != childNodes2.size) {
            seemsEquals = false
          } else {
            // Adds all child nodes to the pending node list.
            childNodes1.lazyZip(childNodes2).foreach((childNodePair: (NewNode, NewNode)) => {
              pendingNodes.push(childNodePair)
            })
          }
        }
      }
    }
    seemsEquals
  }
}
