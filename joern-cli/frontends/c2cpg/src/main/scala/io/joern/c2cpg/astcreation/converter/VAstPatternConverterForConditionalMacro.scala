package io.joern.c2cpg.astcreation.converter

import io.circe.syntax.*
import io.circe.generic.auto.*
import io.circe.parser.*
import io.joern.c2cpg.astcreation.VAstCreatorNew
import io.joern.x2cpg.{Ast, AstEdge}
import io.shiftleft.codepropertygraph.generated.ControlStructureTypes
import io.shiftleft.codepropertygraph.generated.nodes.{AstNodeNew, NewBlock, NewControlStructure, NewNode}
import superc.core.PresenceConditionManager.PresenceCondition
import xtc.tree.{GNode, Node}

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
  
  override def convert(superCVAst: Node): Option[Seq[Ast]] = {
    val handler: ((Node) => Seq[Ast]) = (node: Node) => converter.convert(node)
    val asts: Seq[Ast] = handelConditional(superCVAst, handler, handler)
    Option(asts)
  }

  private def getLocationInformation(node: NewNode): (Int, Int) = {
    val nodeProperties: Map[String, Any] = node.properties
    val line: Int = nodeProperties(LINE_NUMBER_PROPERTY).asInstanceOf[Int]
    val column: Int = nodeProperties(COLUMN_NUMBER_PROPERTY).asInstanceOf[Int]
    (line, column)
  }

  def createConditionalSuperCSubtree(conditionalNode: Node,
                                     firstConditionSubtreeCreator: (Node) => Node,
                                     secondConditionSubtreeCreator: (Node) => Node): Node = {
    require(isConditionalNode(conditionalNode),
            s"It as a \"Conditional\" node expected, but a \"${conditionalNode.getName}\" node was passed")

    // Handels the first condition
    val newSuperCSubtree: Node = GNode.create("Conditional", conditionalNode.size)
    newSuperCSubtree.add(FIRST_CONDITION_INFORMATION, conditionalNode.get(FIRST_CONDITION_INFORMATION))
    val firstConditionalSubtree: Node = firstConditionSubtreeCreator(conditionalNode.getNode(FIRST_CONDITION_SUBTREE))
    newSuperCSubtree.add(FIRST_CONDITION_SUBTREE, firstConditionalSubtree)

    if (conditionalNode.size == FULL_CONDITIONAL_MACRO) {
      // If it is an IF-ELSE conditional node.
      newSuperCSubtree.add(SECOND_CONDITION_INFORMATION, conditionalNode.get(SECOND_CONDITION_INFORMATION))
      val secondConditionalSubtree: Node = secondConditionSubtreeCreator(conditionalNode.getNode(SECOND_CONDITION_SUBTREE))
      newSuperCSubtree.add(SECOND_CONDITION_SUBTREE, firstConditionalSubtree)
    }

    newSuperCSubtree
  }

  def handelConditional(conditionalNode: Node,
                        firstConditionSubtreeCreator: (Node) => Seq[Ast],
                        secondConditionSubtreeCreator: (Node) => Seq[Ast]): Seq[Ast] = {
    require(isConditionalNode(conditionalNode),
            s"It as a \"Conditional\" node expected, but a \"${conditionalNode.getName}\" node was passed")

    var firstCondition: String = conditionalNode.get(FIRST_CONDITION_INFORMATION)
                                                .asInstanceOf[PresenceCondition]
                                                .toString

    if (firstCondition == NO_CONDITION) {
      // If the condition of the conditional node is always true => ignore conditional node
      firstConditionSubtreeCreator(conditionalNode.getNode(FIRST_CONDITION_SUBTREE))

    } else {
      // If the conditional node contains a condition.

      // Extracts first condition.
      var presenceConditionMap: Map[String, String] = Map("AST1" -> firstCondition)

      // Generates the AST of the first condition.
      val firstConditionNode: Node = conditionalNode.getNode(FIRST_CONDITION_SUBTREE)
      val firstConditionSubtrees: Seq[Ast] = firstConditionSubtreeCreator(firstConditionNode)
      var firstConditionSubtree: Ast = combineAsts(firstConditionNode, firstConditionSubtrees)

      // Extracts second condition.
      var secondConditionSubtree: Ast = vAstCreator.AstHelper()
      var secondCondition: String = ""
      if (conditionalNode.size == FULL_CONDITIONAL_MACRO) {
        // If the conditional node contains two subtrees/conditions.
        secondCondition = getSecondCondition(conditionalNode).get
        val secondConditionNode: Node = conditionalNode.getNode(SECOND_CONDITION_SUBTREE)
        secondConditionSubtree = combineAsts(secondConditionNode, secondConditionSubtreeCreator(secondConditionNode))

        // Verschiebt
        if (firstConditionSubtree.root.isEmpty) {
          // If the first condition contains an empty AST.
          firstConditionSubtree = secondConditionSubtree
          secondConditionSubtree = vAstCreator.AstHelper()
          firstCondition = secondCondition
          var presenceConditionMap: Map[String, String] = Map("AST1" -> secondCondition)

        } else {
          // If the first condition contains an AST with nodes.
          presenceConditionMap = presenceConditionMap ++ Map("AST2" -> secondCondition)
        }
      }

      // Checks if the conditional node is required to describe the conditional code.
      var isNecessaryConditionalNode: Boolean = true
      if (firstConditionSubtree.root.isEmpty && secondConditionSubtree.root.isEmpty) {
        // If both sub ASTs ar empty.
        isNecessaryConditionalNode = false

      } else if (secondConditionSubtree.root.isEmpty) {
        // If the conditional node only contains one subtree/condition.

        // Checks if the current conditional Node is necessary or an unnecessary duplicate.
        if (firstConditionSubtree.root.get.nodeKind == JOERN_CONTROL_STRUCTURE_NODE_KIND) {
          val conditionalNode: NewControlStructure = firstConditionSubtree.root.get.asInstanceOf[NewControlStructure]
          if (conditionalNode.controlStructureType == ControlStructureTypes.CHOICE) {
            val conditions: Map[String, String] = decode[Map[String, String]](conditionalNode.presenceCondition) match {
              case Right(map) => map
              case Left(error) =>
                require(1 == 0, s"Failed to parse: $error")
                Map()
            }
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
                                         || ((conditionOptions.size == 1) && (conditionOptions.head.contains(firstCondition)))
          }
        }
      }

      // Creates the conditional/choise node if it is necessary.
      if (isNecessaryConditionalNode) {
        // If the current conditional Node is a required conditional node.

        val firstCodePart: String = firstConditionSubtree.root.get.asInstanceOf[AstNodeNew].code
        val code: String = if (secondConditionSubtree.root.isEmpty) {
          // If the conditional node contains one subtree/condition.
          s"#IFDEF $firstCondition:\n$firstCodePart\n#ENDIF" // TODO: In some cases the "#IFDEF" has to be replaced by "#IF".

        } else {
          // If the conditional node contains two subtrees/conditions.
          val secondCodePart: String = secondConditionSubtree.root.get.asInstanceOf[AstNodeNew].code
          s"#IFDEF $secondCondition:\n$firstCodePart\n#ELSE\n$secondCodePart\n#ENDIF" // TODO: In some cases the "#IFDEF" has to be replaced by "#IF".
        }

        // Creates the conditional node.
        val choiceNode: NewControlStructure = vAstCreator.controlStructureNodeHelper(conditionalNode, ControlStructureTypes.CHOICE, code)

        // Creates the conditional nodes.
        var presenceConditionEdges: Seq[AstEdge] = Seq(AstEdge(choiceNode, firstConditionSubtree.root.get))
        if (secondConditionSubtree.root.isDefined) {
          presenceConditionEdges = presenceConditionEdges :+ AstEdge(choiceNode, secondConditionSubtree.root.get)
        }

        // Adds the presence conditions
        val presenceConditionMapSerialized = presenceConditionMap.asJson.noSpaces
        choiceNode.presenceCondition(presenceConditionMapSerialized)

        // Creates the conditional AST.
        Seq(Ast(
          nodes = Seq(choiceNode) ++ firstConditionSubtree.nodes ++ secondConditionSubtree.nodes,
          edges = firstConditionSubtree.edges ++ secondConditionSubtree.edges ++ presenceConditionEdges,
          conditionEdges = firstConditionSubtree.conditionEdges ++ secondConditionSubtree.conditionEdges, //++ Seq(AstEdge(choiceNode, choiceNode)), //TODO: ++ presenceConditionEdges?
          argEdges = firstConditionSubtree.argEdges ++ secondConditionSubtree.argEdges,
          receiverEdges = firstConditionSubtree.receiverEdges ++ secondConditionSubtree.receiverEdges,
          refEdges = firstConditionSubtree.refEdges ++ secondConditionSubtree.refEdges,
          bindsEdges = firstConditionSubtree.bindsEdges ++ secondConditionSubtree.bindsEdges,
          captureEdges = firstConditionSubtree.captureEdges ++ secondConditionSubtree.captureEdges
        ))

      } else {
        // If the current conditional node only replicated conditions.
        firstConditionSubtrees
      }
    }
  }

  private def combineAsts(rootNode: Node, asts: Seq[Ast]): Ast = {
    asts match {
      case astsSeq if (astsSeq.isEmpty) => vAstCreator.AstHelper()
      case astsSeq if (astsSeq.size == 1) => astsSeq.head
      case astsSeq =>
        val astsOfInterested: Seq[Ast] = astsSeq.filter(ast => ast.nodes.nonEmpty)
        val firstBlockNode: NewNode = astsOfInterested.head.root.get.asInstanceOf[NewNode]

        val line: Int = firstBlockNode.properties("LINE_NUMBER").asInstanceOf[Int]
        val column: Int = firstBlockNode.properties("COLUMN_NUMBER").asInstanceOf[Int]
        val code: String = astsOfInterested.map(ast => ast.root.get.properties("CODE").asInstanceOf[String])
          .mkString("\n")

        val blockNode: NewBlock = vAstCreator.blockNodeHelper(rootNode, code, "<???>", Option(line), Option(column))
        vAstCreator.blockAstHelper(blockNode, astsOfInterested.toList)
    }
  }

  def isConditionalNode(node: Node): Boolean = node.getName.equals("Conditional")

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
}
