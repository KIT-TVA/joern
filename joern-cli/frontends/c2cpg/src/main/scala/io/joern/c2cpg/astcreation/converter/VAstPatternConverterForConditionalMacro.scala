package io.joern.c2cpg.astcreation.converter

import io.circe.syntax.*
import io.circe.generic.auto.*
import io.circe.parser.*
import io.joern.c2cpg.astcreation.VAstCreatorNew
import io.joern.x2cpg.{Ast, AstEdge}
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, nodes}
import io.shiftleft.codepropertygraph.generated.nodes.{AstNodeNew, NewBlock, NewControlStructure, NewNode}
import superc.core.PresenceConditionManager.PresenceCondition
import xtc.tree.{GNode, Node}

import scala.collection.immutable.Queue

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

  def createConditionalSuperCSubtree(conditionalNode: Node, converterState: VAstConverterState,
                                     conditionSubtreeCreator: (Node, VAstConverterState) => Node): Node = {
    require(isConditionalNode(conditionalNode),
      s"It as a \"Conditional\" node expected, but a \"${conditionalNode.getName}\" node was passed")

    // Extents the passed conditionalSubtrreeCreator to also handel multiple consecutive conditional nodes.
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
      newSuperCSubtree.add(SECOND_CONDITION_SUBTREE, secondConditionalSubtree)
    }

    newSuperCSubtree
  }

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
      var secondConditionalSubAst: Ast = vAstCreator.AstHelper()
      if (conditionalNode.size == FULL_CONDITIONAL_MACRO) {
        // If the conditional node contains two subtrees/conditions.
        secondConditionalSubAst = combineAsts(secondConditionalSubtree,
          conditionalHandler(secondCondition, secondConditionalSubtree, converterState))

        // Verschiebt
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

      } else {
        // If the current conditional node only replicated conditions.
        firstConditionalSubAsts
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
        val line: Option[Int] = if (properties.containsKey("LINE_NUMBER")) {
          Option(properties.get("LINE_NUMBER").asInstanceOf[Int])
        } else None
        val column: Option[Int] = if (properties.containsKey("LINE_NUMBER")) {
          Option(properties.get("LINE_NUMBER").asInstanceOf[Int])
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

    // Extents the passed conditionalSubtrreeCreator to also handel multiple consecutive conditional nodes.
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
            val presenceCondition: Map[String, String] = getPressenceConditions(choiceNode)
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

    firstConditionalSubAsts ++ secondConditionalSubAsts
  }

  private def combineAndSimplyTwoConditions(firstConditions: String, secondCondition: String): String = {
    val firstConditionsParts: Seq[String] = firstConditions.split(" \\|\\| ")
    val secondConditionParts: Seq[String] = secondCondition.split(" \\|\\| ")
    firstConditionsParts.flatMap((firstPart: String) => {
      val firstPartSubParts: Seq[String] = firstPart.split( " && ")
      secondConditionParts.map((secondPart: String) => {
        (firstPartSubParts ++ secondPart.split(" && ")).distinct.sorted.mkString(" && ")
      })
    }).mkString(" \\|\\| ")
  }

  private def combineAndSimplyConditions(conditions: Seq[String]): String = {
    println(s"CONDITIONAL INPUT: $conditions")
    conditions.size match {
      case 0 => ""
      case 1 => conditions.head
      case _ =>
        var combinedConditions: String = conditions.head
        for (condition <- conditions.tail) {
          println(s"Handel: \"$condition\"")
          println(s"all: $combinedConditions")
          combinedConditions = combineAndSimplyTwoConditions(combinedConditions, condition)
        }
        println(s"CONDITION-STRING-FINAL: ${combinedConditions.split(" \\|\\| ").distinct.mkString(" \\|\\| ")}")
        combinedConditions.split(" \\|\\| ").distinct.mkString(" \\|\\| ")
    }
  }

  private def getPressenceConditions(conditionalNode: NewControlStructure): Map[String, String] = {
    decode[Map[String, String]](conditionalNode.presenceCondition) match {
      case Right(map) => map
      case Left(error) =>
        require(1 == 0, s"Failed to parse: $error")
        Map()
    }
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

    } else if (secondConditionalSubAst.root.isEmpty) {
      // If the conditional node only contains one subtree/condition.

      // Checks if the current conditional Node is necessary or an unnecessary duplicate.
      if (firstConditionalSubAst.root.get.nodeKind == JOERN_CONTROL_STRUCTURE_NODE_KIND) {
        val conditionalNode: NewControlStructure = firstConditionalSubAst.root.get.asInstanceOf[NewControlStructure]
        if (conditionalNode.controlStructureType == ControlStructureTypes.CHOICE) {
          val conditions: Map[String, String] = getPressenceConditions(conditionalNode)
          val conditionsStrings: Seq[String] = conditions.toSeq
            .filter((key, value) => key.equals("AST1") || key.equals("AST2"))
            .map((key, value) => value)

          // Extracts all interesting conditional options.
          val conditionOptions: Seq[String] = if (conditionsStrings.size == 1) {
            Seq(conditionsStrings.head)
          } else {
            Seq(s"${conditionsStrings.head}||${conditionsStrings.last}",
              s"${conditionsStrings.head} || ${conditionsStrings.last}",
              s"${conditionsStrings.last}||${conditionsStrings.head}",
              s"${conditionsStrings.last} || ${conditionsStrings.head}")
          }
          isNecessaryConditionalNode = (!conditionOptions.exists(condition => condition.equals(firstCondition)))
            || ((conditionOptions.size == 1) && conditionOptions.head.contains(firstCondition))
        }
      }
    }
    isNecessaryConditionalNode
  }

  private def createConditionalNode(conditionalNode: Node, firstCondition: String, firstConditionSubtree: Ast,
                                    secondCondition: String = "", secondConditionSubtree: Ast = vAstCreator.AstHelper()): Ast = {
    /// Cheks the requierments.
    require((firstCondition != null) && (firstConditionSubtree != null) && firstConditionSubtree.root.isDefined,
      "A conditional Node can only be created if at least the firest subtree and condition is defined")

    require((secondCondition.equals("") && ((secondConditionSubtree == null) || secondConditionSubtree.root.isEmpty))
      || ((!secondCondition.equals("")) && (secondConditionSubtree != null) && secondConditionSubtree.root.isDefined),
      "If a conditional node with two conditions is desired, both the second condition and a second AST hast to be passed.")

    val firstCodePart: String = firstConditionSubtree.root.get.asInstanceOf[AstNodeNew].code
    val code: String = if ((secondConditionSubtree == null) || secondConditionSubtree.root.isEmpty) {
      // If the conditional node contains one subtree/condition.
      s"#IFDEF $firstCondition:\n$firstCodePart\n#ENDIF" // TODO: In some cases the "#IFDEF" has to be replaced by "#IF".

    } else {
      // If the conditional node contains two subtrees/conditions.
      val secondCodePart: String = secondConditionSubtree.root.get.asInstanceOf[AstNodeNew].code
      s"#IFDEF $secondCondition:\n$firstCodePart\n#ELSE\n$secondCodePart\n#ENDIF" // TODO: In some cases the "#IFDEF" has to be replaced by "#IF".
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
}
