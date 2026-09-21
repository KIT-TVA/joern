package io.joern.c2cpg.astcreation.converter

import io.circe.syntax.*
import io.circe.parser.*
import io.joern.c2cpg.astcreation.VAstCreatorNew
import io.joern.x2cpg.{Ast, AstEdge}
import io.shiftleft.codepropertygraph.generated.ControlStructureTypes
import io.shiftleft.codepropertygraph.generated.nodes.{AstNodeNew, NewBlock, NewControlStructure, NewNode}
import superc.core.PresenceConditionManager.PresenceCondition
import xtc.tree.{GNode, Node}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters.MapHasAsScala

class VAstConditionalHandler(vAstCreator: VAstCreatorNew, converter: VAstConverter)
  extends VAstHandler(vAstCreator, converter) {
  
  private val SIMPLE_CONDITIONAL_MACRO: Int = 2
  private val FULL_CONDITIONAL_MACRO: Int = 4

  private val FIRST_CONDITION_INFORMATION: Int = 0
  private val FIRST_CONDITION_SUBTREE: Int = 1
  private val SECOND_CONDITION_INFORMATION: Int = 2
  private val SECOND_CONDITION_SUBTREE: Int = 3
  
  private val SUPERC_CONDITIONAL_NODE_NAME: String = "Conditional"

  private val NO_CONDITION: String = "1"
  private val JOERN_CONTROL_STRUCTURE_NODE_KIND: Short = 11
  private val JOERN_BLOCK_NODE_KIND: Short = 6
  private val JOERN_CONTROL_STRUCTURE_NODE_LABEL: String = "CONTROL_STRUCTURE"

  private val LINE_NUMBER_PROPERTY: String = "LINE_NUMBER"
  private val COLUMN_NUMBER_PROPERTY: String = "COLUMN_NUMBER"
  private val CODE_PROPERTY: String = "CODE"

  override def getInitialConverterState: Any = Seq.empty[String]

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
    require(isSuperCConditionalNode(conditionalNode),
            s"It as a \"Conditional\" node expected, but a \"${conditionalNode.getName}\" node was passed")

    // Extents the passed conditionalSubtreeCreator(...) to also handle multiple consecutive conditional nodes.
    val conditionalHandler: (String, Node, VAstConverterState) => Node = (condition: String, node: Node, state: VAstConverterState) => {
      // This is not necessary, but it is done to avoid unexpected side effects.
      val newConverterState: VAstConverterState = getFullConditions(condition, state)

      if (isSuperCConditionalNode(node)) {
        createConditionalSuperCSubtree(node, newConverterState, conditionSubtreeCreator)
      } else {
        conditionSubtreeCreator(node, newConverterState)
      }
    }

    // Handles the first condition
    val newSuperCSubtree: Node = GNode.create(SUPERC_CONDITIONAL_NODE_NAME, conditionalNode.size)
    newSuperCSubtree.add(FIRST_CONDITION_INFORMATION, conditionalNode.get(FIRST_CONDITION_INFORMATION))
    val firstConditionalSubtree: Node = conditionalHandler(getFirstSuperCCondition(conditionalNode),
                                                           conditionalNode.getNode(FIRST_CONDITION_SUBTREE),
                                                           converterState)
    newSuperCSubtree.add(FIRST_CONDITION_SUBTREE, firstConditionalSubtree)

    if (conditionalNode.size == FULL_CONDITIONAL_MACRO) {
      // If it is an IF-ELSE conditional node.
      newSuperCSubtree.add(SECOND_CONDITION_INFORMATION, conditionalNode.get(SECOND_CONDITION_INFORMATION))
      val secondConditionalSubtree: Node = conditionalHandler(getSecondSuperCCondition(conditionalNode).get,
                                                              conditionalNode.getNode(SECOND_CONDITION_SUBTREE),
                                                              converterState)
      newSuperCSubtree.add(SECOND_CONDITION_SUBTREE, secondConditionalSubtree)
    }

    newSuperCSubtree
  }

  /**
   * Translates the passed SuperC conditional node and all consecutive conditional nodes into the JOERN representation.
   * The translation skips SuperC sub-ASTs with unsatisfiable conditions and removes unnecessary conditional nodes. A
   * conditional node is unnecessary if the AST where the conditional node is replaced by the concatenated sub-ASTs of
   * the conditional node describes the same conditional functionality as the conditional node. That means all
   * conditional nodes with tautological conditions or repeated conditions of a parent conditional node are removed.
   * 
   * **Important:**
   * It is not guaranteed that all unsatisfiable conditions are detected, because in some situations satisfiability
   * depends on algebraic expressions.
   *
   * @param conditionalNode         The conditional root node that need to be translated.
   * @param converterState          The current converter state.
   * @param conditionSubtreeCreator The conditional handler that should be called for the translation of the SuperC ASTs
   *                                subtrees.
   *
   * @return Returns the translated JOERN VAST subtree as a sequence. The returned sequence contains only more than one
   *         AST if the passed conditional node is not required for the description on the conditional functionality.
   *         The returned sequence can also be empty if the conditions of the passed conditional node are not
   *         satisfiable.
   */
  def handleConditional(conditionalNode: Node, converterState: VAstConverterState,
                        conditionSubtreeCreator: (Node, VAstConverterState) => Seq[Ast]): Seq[Ast] = {
    require(isSuperCConditionalNode(conditionalNode),
      s"It as a \"Conditional\" node expected, but a \"${conditionalNode.getName}\" node was passed")

    val logicHandler: VAstLogicHandler = converter.getLogicHandler

    // Extends the passed conditionalSubtrreeCreator to also multiple consecutive conditional nodes.
    val conditionalHandler: (String, Node, VAstConverterState) => Seq[Ast] = (condition: String, node: Node, state: VAstConverterState) => {
      val newConverterState: VAstConverterState = getFullConditions(condition, state)
      if (isSuperCConditionalNode(node)) {
        handleConditional(node, newConverterState, conditionSubtreeCreator)
      } else {
        conditionSubtreeCreator(node, newConverterState)
      }
    }

    // Extracts conditions and subtrees. The first sub AST always contains a sub AST if the conditional SuperC node
    // contains at least one sub AST with a satisfiable condition.
    val (firstCondition: String, firstConditionalSubtree, secondCondition: String, secondConditionalSubtree) =
      extractConditionsAndSubtrees(conditionalNode, converterState)

    if (!logicHandler.isSatisfiable(firstCondition)) {
      // If the current SuperC conditional node does not contain at least one satisfiable condition.
      Seq.empty[Ast]

    } else {
      // If the current SuperC conditional node contains at least one satisfiable condition.

      // Generates the ASTs of the first condition.
      val firstConditionalSubAsts: Seq[Ast] = conditionalHandler(firstCondition, firstConditionalSubtree, converterState)

      if (logicHandler.isTautology(firstCondition)) {
        // If the condition of the first conditional node is always true => ignore conditional node
        firstConditionalSubAsts

      } else {
        // If the conditional node contains a normal condition.

        // Generates the AST of the first condition.
        val firstConditionalSubAst: Ast = combineAsts(firstConditionalSubtree, firstConditionalSubAsts)

        // Extracts second condition.
        var secondConditionalSubAsts: Seq[Ast] = Seq.empty[Ast]
        var secondConditionalSubAst: Ast = vAstCreator.AstHelper()
        if (logicHandler.isSatisfiable(secondCondition)) {
          // If the conditional node contains two satisfiable subtrees/conditions.
          secondConditionalSubAsts = conditionalHandler(secondCondition, secondConditionalSubtree, converterState)
          secondConditionalSubAst = combineAsts(secondConditionalSubtree, secondConditionalSubAsts)
        }

        // Checks if the conditional node is required to describe the conditional code.
        if (isNecessaryCondition(firstCondition, firstConditionalSubAst, secondCondition, secondConditionalSubAst)) {
          // If the current conditional Node is a required conditional node.
          Seq(createConditionalNode(conditionalNode, firstCondition, firstConditionalSubAst,
                                    secondCondition, secondConditionalSubAst))

        } else if (!logicHandler.isSatisfiable(secondCondition)) {
          // If the current conditional node only replicated conditions and only the first condition is satisfiable.
          firstConditionalSubAsts

        } else {
          // If the current conditional node only replicated conditions and both conditions are satisfiable.
          sortAstsByCodPosition(firstConditionalSubAsts ++ secondConditionalSubAsts)
        }
      }
    }
  }

  /**
   * Translates the passed SuperC conditional node and all consecutive conditional nodes into the JOERN representation.
   * The translation skips SuperC sub-ASTs with unsatisfiable conditions and removes unnecessary conditional nodes. A
   * conditional node is unnecessary if the AST where the conditional node is replaced by the concatenated sub-ASTs of
   * the conditional node describes the same conditional functionality as the conditional node. That means all
   * conditional nodes with tautological conditions or repeated conditions of a parent conditional node are removed.
   *
   * **Important:**
   * It is not guaranteed that all unsatisfiable conditions are detected, because in some situations satisfiability
   * depends on algebraic expressions.
   *
   * @param conditionalNode         The conditional root node that need to be translated.
   * @param converterState          The current converter state.
   * @param conditionSubtreeCreator The conditional handler that should be called for the translation of the SuperC ASTs
   *                                subtrees.
   *
   * @return Returns the translated JOERN VAST subtree as a sequence. The returned sequence contains only unconditional
   *         ASTs (normal ASTs) or ASTs with a JOERN choice node as the root node that has only one unconditional sub
   *         AST. The returned sequence can also be empty if the conditions of the passed conditional node are not
   *         satisfiable.
   */
  def handleAndSimplifyConditional(conditionalNode: Node, converterState: VAstConverterState,
                                   conditionSubtreeCreator: (Node, VAstConverterState) => Seq[Ast]): Seq[Ast] = {
    require(isSuperCConditionalNode(conditionalNode),
      s"It as a \"Conditional\" node expected, but a \"${conditionalNode.getName}\" node was passed")

    val logicHandler: VAstLogicHandler = converter.getLogicHandler

    // Extends the passed conditionalSubtrreeCreator(...) to also handle multiple consecutive conditional nodes.
    val conditionalAstHandler: (String, Node, VAstConverterState) => Seq[Ast] = (condition: String, node: Node, state: VAstConverterState) => {
      val newConverterState: VAstConverterState = getFullConditions(condition, state)
      if (isSuperCConditionalNode(node)) {
        // Recursive handling of consecutive conditional node.
        handleAndSimplifyConditional(node, newConverterState, conditionSubtreeCreator)

      } else {
        // Creation and handling of sub ASTs.
        conditionSubtreeCreator(node, newConverterState).map(subAst => {
          val rootNode: Option[NewNode] = subAst.root
          if (rootNode.isDefined && isJoernChoiceNode(rootNode.get)) {
            // If the root node of the sub AST is a conditional/chiose node.

            // Returns the sub AST. A modification of the root choice node is not necessary because the condition of the
            // choice node always also incloud all conditions of the parent conditional/choice nodes.
            subAst

          } else {
            // If the root node of th sub AST is a normale node.

            // The passed condition is, by construction, the simplified condition that already contains all parent
            // conditions. So no modification is required to the passed condition.
            createConditionalNode(conditionalNode, condition, subAst)
          }
        })
      }
    }

    // Extracts conditions and subtrees. The first sub AST always contains a sub AST if the conditional SuperC node
    // contains at least one sub AST with a satisfiable condition.
    val (firstCondition: String, firstConditionalSubtree: Node, secondCondition, secondConditionalSubtree) =
      extractConditionsAndSubtrees(conditionalNode, converterState)

    if (!logicHandler.isSatisfiable(firstCondition)) {
      // If the current SuperC conditional node does not contain at least one satisfiable condition.
      Seq.empty[Ast]
      
    } else {
      // If the current SuperC conditional node contains at least one satisfiable condition.

      // Converts the SuperC subtrees.
      val firstConditionalSubAsts: Seq[Ast] = conditionalAstHandler(firstCondition, firstConditionalSubtree, converterState)
      val secondConditionalSubAsts: Seq[Ast] = if (secondConditionalSubtree != null && logicHandler.isSatisfiable((secondCondition))) {
        conditionalAstHandler(secondCondition, secondConditionalSubtree, converterState)
      } else Seq.empty[Ast]
      
      // Combine duplicated sub ASTs.
      val allSubAsts: Seq[Ast] = firstConditionalSubAsts ++ secondConditionalSubAsts
      combineAndSimplify(allSubAsts)
    }
  }

  def handleAndSimplifyConditionalExtended(conditionalNode: Node, converterState: VAstConverterState,
                                           conditionSubtreesCreator: (Node, VAstConverterState) => Seq[Ast]): Seq[Ast] =
    if (isSuperCConditionalNode(conditionalNode)) {
      handleAndSimplifyConditional(conditionalNode, converterState, conditionSubtreesCreator)
    } else conditionSubtreesCreator(conditionalNode, converterState)

  /**
   * Checks if the passed SuperC node is a conditional node.
   *
   * @param node The SuperC node that should be checked for whether it is a conditioal node is.
   * @return Returns `true` if the passed superC node is a conditional node, otherwise `false` is returned.
   */
  def isSuperCConditionalNode(node: Node): Boolean =
    node.isInstanceOf[GNode] && node.getName.equals(SUPERC_CONDITIONAL_NODE_NAME)

  /**
   * Checks if the passed JOERN node is a conditional/choise node.
   *
   * @param node The JOERN node that should be checked for whether it is a conditioal/choice node is.
   * @return Returns `true` if the passed JOERN node is a conditional/choice node, otherwise `false` is returned.
   */
  def isJoernChoiceNode(node: NewNode): Boolean = (node.nodeKind == JOERN_CONTROL_STRUCTURE_NODE_KIND)
    && node.label.equals(JOERN_CONTROL_STRUCTURE_NODE_LABEL)
    && node.asInstanceOf[NewControlStructure].controlStructureType.equals(ControlStructureTypes.CHOICE)

  def getFirstSuperCCondition(node: Node): String = {
    require(isSuperCConditionalNode(node),
            "A conditional node was expected, but a node of a different node type was passed.")
    node.get(FIRST_CONDITION_INFORMATION).asInstanceOf[PresenceCondition].toString
  }

  def getSecondSuperCCondition(node: Node): Option[String] = {
    require(isSuperCConditionalNode(node),
            "A conditional node was expected, but a node of a different node type was passed.")
    if (node.size == FULL_CONDITIONAL_MACRO) {
      Option(node.get(SECOND_CONDITION_INFORMATION).asInstanceOf[PresenceCondition].toString)
    } else None
  }

  def getFirstJoernPresenceConditions(choiceNode: NewControlStructure): String =
    getPresenceConditions(choiceNode)("AST1")

  def getSecondJoernPresenceConditions(choiceNode: NewControlStructure): Option[String] =
    getPresenceConditions(choiceNode).get("AST2")

  def getFirstSuperCConditionalSubtree(node: Node): Node = {
    require(isSuperCConditionalNode(node),
            "A conditional node was expected, but a node of a different node type was passed.")
    node.getNode(FIRST_CONDITION_SUBTREE)
  }

  def getSecondSuperCConditionalSubtree(node: Node): Option[Node] = {
    require(isSuperCConditionalNode(node),
            "A conditional node was expected, but a node of a different node type was passed.")
    if (node.size == FULL_CONDITIONAL_MACRO) {
      Option(node.getNode(SECOND_CONDITION_SUBTREE))
    } else {
      None
    }
  }

  def getAllSuperCConditionalAstSubtrees(node: Node): Seq[Node] = {
    if (isSuperCConditionalNode(node)) {
      if (node.size == SIMPLE_CONDITIONAL_MACRO) {
        getAllSuperCConditionalAstSubtrees(node.getNode(FIRST_CONDITION_SUBTREE))
      } else {
        getAllSuperCConditionalAstSubtrees(node.getNode(FIRST_CONDITION_SUBTREE))
          ++ getAllSuperCConditionalAstSubtrees(node.getNode(SECOND_CONDITION_SUBTREE))
      }
    } else {
      Seq(node)
    }
  }

  /**
   * Adds the new condition to the condition stack of parent conditions and return the updated converter state.
   *
   * @param newCondition   The new condition.
   * @param converterState The current converter state.
   * @return Returns the updated converter state.
   */
  private def getFullConditions(newCondition: String, converterState: VAstConverterState): VAstConverterState = {
    val logicHandler: VAstLogicHandler = converter.getLogicHandler
    if (logicHandler.isTautology(newCondition)) converterState else {
      val parentConditions: Seq[String] = converterState.getState(this).asInstanceOf[Seq[String]]
      converterState.updateState(this, parentConditions ++ Seq(newCondition))
    }
  }

  /**
   * Combines the passed list of ASTs into one AST that has a code block node as a root node if more than one AST is
   * passed. In case that only one AST is passed, the passed  AST is returned without any changes. If no AST is passed
   * an empty AST is returned.
   *
   * This Methodes garnties the code position order.
   *
   * @param rootNode the SuperC node that "contains" all passed ASTs as a sub AST.
   * @param asts     The list of ASTs to be combined into a single AST.
   * @return Retrurns an empty AST or a AST that comtains all passed  ASTs as subtree.
   */
  private def combineAsts(rootNode: Node, asts: Seq[Ast]): Ast = {
    asts match {
      case astsSeq if astsSeq.isEmpty => vAstCreator.AstHelper()
      case astsSeq if astsSeq.size == 1 => astsSeq.head
      case astsSeq =>
        val astsOfInterested: Seq[Ast] = astsSeq.filter(ast => ast.nodes.nonEmpty)
        val sortedAstOfInterested: Seq[Ast] = sortAstsByCodPosition(astsOfInterested)

        // Determines the code position information.
        val firstBlockNode: NewNode = sortedAstOfInterested.head.root.get
        val properties = firstBlockNode.propertiesMap
        val line: Option[Int] = if (properties.containsKey(LINE_NUMBER_PROPERTY)) {
          Option(properties.get(LINE_NUMBER_PROPERTY).asInstanceOf[Int])
        } else None
        val column: Option[Int] = if (properties.containsKey(COLUMN_NUMBER_PROPERTY)) {
          Option(properties.get(COLUMN_NUMBER_PROPERTY).asInstanceOf[Int])
        } else None

        // Creates the code block code.
        val code: String = sortedAstOfInterested.map(ast => ast.root.get.properties("CODE").asInstanceOf[String])
          .mkString("\n")

        // Creates the root coe block node and the combined AST.
        val blockNode: NewBlock = vAstCreator.blockNodeHelper(rootNode, s"{$code}", "void", line, column)
        vAstCreator.blockAstHelper(blockNode, sortedAstOfInterested.toList)
    }
  }

  /**
   * Sorts the AST sequence by the first code position of each AST.
   *
   * This method can handle JOERN root-choice nodes.
   *
   * @param asts The ASTs that should be sorted by their first code position.
   * @return Returns the sorted ASTs as sequence.
   */
  private def sortAstsByCodPosition(asts: Seq[Ast]): Seq[Ast] = asts.sortWith((ast1: Ast, ast2: Ast) => {
    // Extracts the position information.
    val position1: Option[(Int, Int)] = getFirstAstCodePosition(ast1)
    val position2: Option[(Int, Int)] = getFirstAstCodePosition(ast2)

    // Normalizes the position information and handles missing code position information.
    val (line1: Int, column1: Int) = if (position1.isDefined) position1.get else (Int.MaxValue, Int.MaxValue)
    val (line2: Int, column2: Int) = if (position2.isDefined) position2.get else (Int.MaxValue, Int.MaxValue)

    // Compare expression
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
      // If the passed  AST does not contain at least one node.
      None

    } else {
      // Get first non-conditional node position.
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
          if (line.isDefined
            && (firstLine > line.get || (firstLine == line.get && column.isDefined && firstColumn > column.get))) {
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
   * Returns the presence conditions of the passed JOERN choice node.
   *
   * @param conditionalNode The JOERN choice node.
   * @return Returns the presence conditions of the passed JOERN choice node as a map containing both conditions if the
   *         choice node has two conditional sub ASTs.
   */
  private def getPresenceConditions(conditionalNode: NewControlStructure): Map[String, String] = {
    require(isJoernChoiceNode(conditionalNode),
            "A JOERN choice node was expected, but a node of a different node type was passed.")

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

  /**
   * Returns the simplified conditions with all parent conditions together with the corresponding SuperC sub AST of the
   * passed conditional SuperC node.
   *
   * @param conditionalNode The parent SuperC conditional node of the conditional sub ASTs that should be returned
   *                        together with their corresponding simplified conditions.
   * @param converterState  The current converter state.
   * @return Returns the simplified conditions with all parent conditions together with the corresponding SuperC sub AST
   *         of the passed conditional SuperC node. If the passed SuperC conditional node only contains one conditional
   *         sub AST with a satisfiable condition the second condition is set to `"0"` and `null` is returned for the
   *         second sub AST.
   */
  private def extractConditionsAndSubtrees(conditionalNode: Node,
                                           converterState: VAstConverterState): (String, Node, String, Node) = {
    require(isSuperCConditionalNode(conditionalNode),
            s"It as a \"Conditional\" node expected, but a \"${conditionalNode.getName}\" node was passed.")

    val logicHandler: VAstLogicHandler = converter.getLogicHandler
    val allParentConditions: Seq[String] = converterState.getState(this).asInstanceOf[Seq[String]]

    // Extracts the first condition and its AST.
    val firstCondition: String = getFirstSuperCCondition(conditionalNode)
    var firstConditionalSubtree: Node = getFirstSuperCConditionalSubtree(conditionalNode)

    // Combines and simplified the condition of the first sub AST.
    var firstSimplifiedCondition: String = logicHandler.combineAndSimplyConditionsAnd(allParentConditions ++ Seq(firstCondition))

    // Extracts the simplified second condition and its AST if defined.
    var secondSimplifiedCondition: String = if (conditionalNode.size == FULL_CONDITIONAL_MACRO) {
      val secondCondition: String = getSecondSuperCCondition(conditionalNode).get

      // Combines and simplified the condition of the second sub AST.
      logicHandler.combineAndSimplyConditionsAnd(allParentConditions ++ Seq(secondCondition))
    } else "0" // If the conditional node only contains one sub AST.
    var secondConditionalSubtree: Node = if (conditionalNode.size == FULL_CONDITIONAL_MACRO) {
      getSecondSuperCConditionalSubtree(conditionalNode).get
    } else null // If the conditional node only contains one sub AST.

    // Check if the first condition is not satisfiable.
    if (!logicHandler.isSatisfiable(firstSimplifiedCondition)) {
      // Replaces the first condition sub AST by the second conditional sub AST if the condition of the first sub AST is
      // not satisfiable.
      firstSimplifiedCondition = secondSimplifiedCondition
      firstConditionalSubtree = secondConditionalSubtree
      secondSimplifiedCondition = "0"
      secondConditionalSubtree = null
    }
    (firstSimplifiedCondition, firstConditionalSubtree, secondSimplifiedCondition, secondConditionalSubtree)
  }

  /**
   * Checks if a conditional Node is required to description the functionality of the code.
   *
   * @param firstCondition          The conditional string of the first condition.
   * @param firstConditionalSubAst  The converted AST of the first condition.
   * @param secondCondition         The conditional string of the second condition or "" if no second condition exist.
   * @param secondConditionalSubAst The converted AST of the second condition or an empty AST if no second condition
   *                                exist.
   * @return Returns `true` if the conditional node is necessary to conserve the conditional description of the code and
   *         `false` if the conditional node is only a condition repetition of a conditional node above in the VAST tree
   *         or a tautology.
   */
  private def isNecessaryCondition(firstCondition: String, firstConditionalSubAst: Ast,
                                   secondCondition: String, secondConditionalSubAst: Ast): Boolean = {
    val logicHandler: VAstLogicHandler = converter.getLogicHandler

    // Checks if the conditional node is required to describe the conditional code.
    var isNecessaryConditionalNode: Boolean = true
    if (firstConditionalSubAst.root.isEmpty && secondConditionalSubAst.root.isEmpty) {
      // If both sub ASTs ar empty.
      isNecessaryConditionalNode = false

    } else if (logicHandler.isTautology(firstCondition) && logicHandler.isTautology(secondCondition)) {
      // If the defined conditions are always satisfied.
      isNecessaryConditionalNode = false

    } else {
      // Checks if both child sub ASTs consists of on block that contains only conditional/choice node.
      // This check is relevant, because each choice node contains also all parent condition constrains, so that the
      // current condition does not need converted if all AST root nodes are choice nodes or a root code block node with
      // only choice nodes as child nodes.
      var allChoiceNodes: Boolean = false

      // Checks the first child AST if the conditional node is required (the first AST "only" contains choice node).
      val firstAstRootNode: NewNode = firstConditionalSubAst.root.get
      if (isJoernChoiceNode(firstAstRootNode)) {
        // If the root node of the first child AST is a conditional node.
        allChoiceNodes = true

      } else if (firstAstRootNode.nodeKind == JOERN_BLOCK_NODE_KIND) {
        // If the root node of the child AST is a block node.
        allChoiceNodes = firstConditionalSubAst.edges
          .filter((edge: AstEdge) => edge.src == firstAstRootNode) // All outgoing edges of the root code block.
          .forall((edge: AstEdge) => isJoernChoiceNode(edge.dst))  // Checks if the root code block only contains choice nodes.
      }

      if (allChoiceNodes) {
        // If the first condition AST only contains choice node
        if ((secondConditionalSubAst == null) || secondConditionalSubAst.root.isEmpty) {
          // If only the first condition is defined.
          isNecessaryConditionalNode = false

        } else {
          // If the current conditional SuperC nodes also contain a second condition.

          // Checks the second child AST if the conditional node is required (the second AST "only" contains choice node).
          val secondAstRootNode: NewNode = secondConditionalSubAst.root.get
          if (isJoernChoiceNode(secondAstRootNode)) {
            // If the root node of the child AST is a conditional node.
            isNecessaryConditionalNode = false

          } else if (secondAstRootNode.nodeKind == JOERN_BLOCK_NODE_KIND) {
            // If the root node of the child AST is a block node.
            isNecessaryConditionalNode = !secondConditionalSubAst.edges
              .filter((edge: AstEdge) => edge.src == secondAstRootNode) // All outgoing edges of the root code block.
              .forall((edge: AstEdge) => isJoernChoiceNode(edge.dst))   // Checks if the root code block only contains choice nodes.
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
                                    secondCondition: String = "0", secondConditionSubtree: Ast = vAstCreator.AstHelper()): Ast = {
    val logicHandler: VAstLogicHandler = converter.getLogicHandler

    // Cheks the requirements.
    require(logicHandler.isSatisfiable(firstCondition) && (firstConditionSubtree != null)
              && firstConditionSubtree.root.isDefined,
            "A conditional Node can only be created if at least the firest subtree and condition is defined")

    require((!logicHandler.isSatisfiable(secondCondition) && ((secondConditionSubtree == null) || secondConditionSubtree.root.isEmpty))
      || (logicHandler.isSatisfiable(secondCondition) && (secondConditionSubtree != null) && secondConditionSubtree.root.isDefined),
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
      conditionEdges = firstConditionSubtree.conditionEdges ++ secondConditionSubtree.conditionEdges, // TODO: ++ presenceConditionEdges?
      argEdges = firstConditionSubtree.argEdges ++ secondConditionSubtree.argEdges,
      receiverEdges = firstConditionSubtree.receiverEdges ++ secondConditionSubtree.receiverEdges,
      refEdges = firstConditionSubtree.refEdges ++ secondConditionSubtree.refEdges,
      bindsEdges = firstConditionSubtree.bindsEdges ++ secondConditionSubtree.bindsEdges,
      captureEdges = firstConditionSubtree.captureEdges ++ secondConditionSubtree.captureEdges
    )
  }

  /**
   * Groups similar ASTs and updates their choice node condition, or removes it if the condition has become a tautology
   * as a result of grouping similar ASTs. Two ASTs are similar if they are equal except for the root choice node, if
   * they have a root choice node.
   * 
   * @param allSubAsts The sequence of ASTs that should be combined and sorted in code order.
   * @return Returns the sequence with the combined ASTs in code order.
   */
  private def combineAndSimplify(allSubAsts: Seq[Ast]): Seq[Ast] = {
    // Annotates all ASTs with its root conditions without considering the parent conditions because all ASTs share the
    // same parent condition.
    val allSubAstsWithCondition: Seq[(String, Ast)] = allSubAsts
      .filter((ast: Ast) => ast.root.isDefined) // Removes oll empty ASTs.
      .map((ast: Ast) => {
        val condition: String = ast.root.get match {
          case choiceNode: NewControlStructure => getPresenceConditions(choiceNode)("AST1")
          case _ => "1" // If it is an unconditional AST.
        }
        (condition, ast)
      })
    
    // Combines ASTs that differ only at the root choice node if the ASTs have a root choice node and updates their
    // condition.
    val logicHandler: VAstLogicHandler = converter.getLogicHandler
    val subAsts: Seq[Ast] = groupSubAsts(allSubAstsWithCondition).map((condition: String, ast: Ast) => {
      if (logicHandler.isTautology(condition)) {
        // If the condition is a tautology.
        // Creates the unconditional conditional AST.
        val rootNode: NewNode = ast.root.get
        Ast(
          nodes = ast.nodes.filterNot((node: NewNode) => node == rootNode),
          edges = ast.edges.filterNot((edge: AstEdge) => (edge.src == rootNode) || (edge.dst == rootNode)),
          conditionEdges = ast.conditionEdges.filterNot((edge: AstEdge) => (edge.src == rootNode) || (edge.dst == rootNode)),
          argEdges = ast.argEdges.filterNot((edge: AstEdge) => (edge.src == rootNode) || (edge.dst == rootNode)),
          receiverEdges = ast.receiverEdges.filterNot((edge: AstEdge) => (edge.src == rootNode) || (edge.dst == rootNode)),
          refEdges = ast.refEdges.filterNot((edge: AstEdge) => (edge.src == rootNode) || (edge.dst == rootNode)),
          bindsEdges = ast.bindsEdges.filterNot((edge: AstEdge) => (edge.src == rootNode) || (edge.dst == rootNode)),
          captureEdges = ast.captureEdges.filterNot((edge: AstEdge) => (edge.src == rootNode) || (edge.dst == rootNode))
        )

      } else {
        // If only the condition has to be updated.
        // Updates the condition.
        val conditionalNode: NewControlStructure = ast.root.get.asInstanceOf[NewControlStructure]
        updatePresenceCondition(conditionalNode, Map("AST1" -> condition))

        // Updates the code in the choice node by updating the root node condition.
        // By design, it is guaranteed that the root choice node always has only one child AST.
        val subtreeCode: String = ast.edges.filter((edge: AstEdge) => edge.src == conditionalNode).head.dst
          .properties(CODE_PROPERTY).asInstanceOf[String]
        conditionalNode.code(s"#IF $condition:\n$subtreeCode\n#ENDIF")

        // If the JOERN choice nodes also contain a code position, this position needs to be updated in some
        // situration/implementations. However, this is not necessary/is not done, because at the moment all JOERN
        // choice node does not provide position information.
        ast
      }
    })

    // Returns the generated sub ASTs in code order
    sortAstsByCodPosition(subAsts)
  }

  private def groupSubAsts(asts: Seq[(String, Ast)]): Seq[(String, Ast)] = {
    if (asts.size <= 1) {
      // If only one AST exist.
      asts

    } else {
      // If multiple ASTs are passed.

      // Combines the conditions of all ASTs that are exactly the same, except the root choice node.
      val groupedSubAsts: ListBuffer[(ListBuffer[String], Ast)] = ListBuffer((ListBuffer(asts.head(0)), asts.head(1)))
      for ((logicString: String, ast: Ast) <- asts.tail) {

        // Checks if the current AST exactly match an already checked AST, except for the root choice node.
        var notAssigned: Boolean = true
        for (index <- groupedSubAsts.indices) {
          if (notAssigned && sameAst(ast, groupedSubAsts(index)(1))) {
            groupedSubAsts(index)(0) += logicString
            notAssigned = false
          }
        }

        // If the current AST does not exactly match an already checked AST.
        if (notAssigned) {
          groupedSubAsts.addOne((ListBuffer(logicString), ast))
        }
      }

      // Simplify conditional logic expression of the grouped Subtrees with.
      val logicHandler: VAstLogicHandler = converter.getLogicHandler
      groupedSubAsts.map((logicString: ListBuffer[String], ast: Ast) => {
        println(s"logic string: \"$logicString\"")

        val simplifiedCombinedExpression: String  = logicHandler.combineAndSimplyConditionsOr(logicString.toSeq)
        (simplifiedCombinedExpression, ast)
      }).toSeq
    }
  }
  
  /**
   * Compares the two given JOERN ASTs node-wise with all node parameters and child nodes, except for the root choice
   * node.
   *
   * @param ast1 First AST to compare. It is expected that the root nocde is a choice node with only one child.
   * @param ast2 Second AST to compare. It is expected that the root nocde is a choice node with only one child.
   * @return Returns `true` if the two given JOERN ASTs are the same, except for the root choice node, otherwise `false`
   *         is returned.
   */
  private def sameAst(ast1: Ast, ast2: Ast): Boolean = {
    // Extracts the normal root node (the first node that is not a choice node).
    val root1: NewNode = getNormalRootNode(ast1)
    val root2: NewNode = getNormalRootNode(ast2)    

    // Iterates over both ASTs simultaneously and compares them node wise.
    var seemsEquals: Boolean = true
    val pendingNodes: mutable.Stack[(NewNode, NewNode)] = mutable.Stack((root1, root2))
    while (seemsEquals && pendingNodes.nonEmpty) {
      val (node1: NewNode, node2: NewNode) = pendingNodes.pop

      // Compares the node kind, node object type and node label.
      if (node1.nodeKind != node2.nodeKind || node1.label != node2.label
        || !node1.getClass.toString.equals(node2.getClass.toString)) {
        // If the Asts diverge at the current node.
        seemsEquals = false

      } else {
        // If the node type of the nodes in both ASTs is the same.

        val nodeProperties1: Map[String, Any] = node1.propertiesMap.asScala.toMap
        val nodeProperties2: Map[String, Any] = node2.propertiesMap.asScala.toMap

        // Checks if the two keys are different.
        val nodePropertyKeys1 = nodeProperties1.keySet
        val nodePropertyKeys2 = nodeProperties1.keySet
        if (!nodePropertyKeys1.subsetOf(nodePropertyKeys2) || !nodePropertyKeys2.subsetOf(nodePropertyKeys1)) {
          // If the nodes do not have the same properties.
          seemsEquals = false

        } else {
          // If the nodes have the same properties.
          // Compares all node properties.
          seemsEquals = nodeProperties1.forall((key: String, value: Any) => value.equals(nodeProperties2(key)))

          // Selects all child nodes of th current two nodes.
          val childNodes1: Seq[NewNode] = ast1.edges.filter((edge: AstEdge) => edge.src.equals(node1)).map((edge: AstEdge) => edge.dst).toSeq
          val childNodes2: Seq[NewNode] = ast2.edges.filter((edge: AstEdge) => edge.src.equals(node2)).map((edge: AstEdge) => edge.dst).toSeq

          if (childNodes1.size != childNodes2.size) {
            // If the two nodes do not have the same number of child nodes.
            seemsEquals = false
          } else {
            // If the two nodes have the same number of child nodes.
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

  private def getNormalRootNode(ast: Ast): NewNode = ast.root.get match {
    case choiceNode if isJoernChoiceNode(choiceNode) =>
      ast.edges.filter((edge: AstEdge) => edge.src.equals(choiceNode)).head.dst // Assumes that the root choice node only contains one condition.
    case normalNode => normalNode
  }
}
