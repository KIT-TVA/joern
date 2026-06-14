package io.joern.c2cpg.astcreation.converter

import io.circe.syntax.*
import io.circe.generic.auto.*
import io.circe.parser._
import io.joern.c2cpg.astcreation.VAstCreatorNew
import io.joern.x2cpg.{Ast, AstEdge}
import io.shiftleft.codepropertygraph.generated.ControlStructureTypes
import io.shiftleft.codepropertygraph.generated.nodes.{AstNodeNew, NewControlStructure, NewNode}
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
    val handler: ((Node) => Ast) = (node: Node) => converter.convert(node).head
    val ast: Ast = handelConditional(superCVAst, handler, handler)
    Option(Seq(ast))
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
    require(!isConditionalNode(conditionalNode),
            s"It as a \"Conditional\" node expected, but a \"${conditionalNode.getName}\" node was passed")

    // Handels the first condition
    val newSuperCSubtree: Node = GNode.create("Conditional", conditionalNode.size)
    val firstConditionalSubtree: Node = firstConditionSubtreeCreator(conditionalNode.getNode(FIRST_CONDITION_SUBTREE))
    newSuperCSubtree.add(FIRST_CONDITION_SUBTREE, firstConditionalSubtree)
    newSuperCSubtree.add(FIRST_CONDITION_INFORMATION, conditionalNode.get(FIRST_CONDITION_INFORMATION))

    if (conditionalNode.size == FULL_CONDITIONAL_MACRO) {
      // If it is an IF-ELSE conditional node.
      val secondConditionalSubtree: Node = secondConditionSubtreeCreator(conditionalNode.getNode(SECOND_CONDITION_SUBTREE))
      newSuperCSubtree.add(SECOND_CONDITION_SUBTREE, firstConditionalSubtree)
      newSuperCSubtree.add(SECOND_CONDITION_INFORMATION, conditionalNode.get(SECOND_CONDITION_INFORMATION))
    }

    newSuperCSubtree
  }

  def handelConditional(conditionalNode: Node,
                        firstConditionSubtreeCreator: (Node) => Ast,
                        secondConditionSubtreeCreator: (Node) => Ast): Ast = {
    require(isConditionalNode(conditionalNode),
            s"It as a \"Conditional\" node expected, but a \"${conditionalNode.getName}\" node was passed")
    converter.convert(conditionalNode.getNode(FIRST_CONDITION_SUBTREE)).head

    val firstPresenceCondition: PresenceCondition = conditionalNode.get(FIRST_CONDITION_INFORMATION)
                                                                   .asInstanceOf[PresenceCondition]

    if (firstPresenceCondition.toString == NO_CONDITION) {
      // If the condition of the conditional node is always true => ignore conditional node
      converter.convert(conditionalNode.getNode(FIRST_CONDITION_SUBTREE)).head

    } else {
      // If the conditional node contains a condition.

      // Extracts first condition.
      val firstCondition: String = firstPresenceCondition.toString
      val firstConditionSubtree: Ast = firstConditionSubtreeCreator(conditionalNode.getNode(FIRST_CONDITION_SUBTREE))
      val firstCodePart: String = firstConditionSubtree.root.get.asInstanceOf[AstNodeNew].code
      var code: String = s"#ifdef $firstCondition:\n$firstCodePart"
      var presenceConditionMap: Map[String, String] = Map("AST1" -> firstCondition)

      // Extracts second condition.
      var secondConditionSubtree: Ast = vAstCreator.AstHelper()
      var isNecessaryConditionalNode: Boolean = true
      if (conditionalNode.size == SIMPLE_CONDITIONAL_MACRO) {
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
            isNecessaryConditionalNode = !conditionOptions.exists(condition => condition.equals(firstCondition))
          }
        }

      } else {
        val secondPresenceCondition: PresenceCondition = conditionalNode.get(SECOND_CONDITION_INFORMATION)
          .asInstanceOf[PresenceCondition]
        val secondCondition: String = secondPresenceCondition.toString
        secondConditionSubtree = secondConditionSubtreeCreator(conditionalNode.getNode(SECOND_CONDITION_SUBTREE))
        val secondCodePart: String = secondConditionSubtree.root.get.asInstanceOf[AstNodeNew].code
        code = code + s"#else\n$secondCodePart"
        presenceConditionMap = presenceConditionMap ++ Map("AST2" -> secondCondition)
      }
      code = code + "\n#endif "

      if (isNecessaryConditionalNode) {
        // If the current conditional Node is a required conditional node.
        // Creates the conditional node.
        val choiceNode: NewControlStructure = vAstCreator.controlStructureNodeHelper(conditionalNode, ControlStructureTypes.CHOICE, code)

        // Creates the conditional nodes.
        var presenceConditionEdges: Seq[AstEdge] = Seq(AstEdge(choiceNode, firstConditionSubtree.root.get))
        if (conditionalNode.size == FULL_CONDITIONAL_MACRO) {
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

      } else {
        // If the current conditional node only replicated conditions.
        firstConditionSubtree
      }
    }
  }

  def isConditionalNode(node: Node): Boolean = node.getName.equals("Conditional")
}
