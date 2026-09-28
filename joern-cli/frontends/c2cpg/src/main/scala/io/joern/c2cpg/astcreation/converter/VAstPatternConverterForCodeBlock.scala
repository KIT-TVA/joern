package io.joern.c2cpg.astcreation.converter

import io.joern.c2cpg.astcreation.VAstCreatorNew
import io.joern.c2cpg.astcreation.converter.VAstConverter
import io.joern.x2cpg.{Ast, AstEdge}
import io.shiftleft.codepropertygraph.generated.nodes.*
import xtc.tree.Node

class VAstPatternConverterForCodeBlock(vAstCreator: VAstCreatorNew, converter: VAstConverter)
  extends VAstPatternConverter(vAstCreator, converter, List.apply("CompoundStatement")) {
  val conditionalHandler: VAstConditionalHandler = converter.getConditionalHandler
  var variableHandler: VAstVariableHandler = converter.getDeclarationHandler

  override def convert(superCVAst: Node, converterState: VAstConverterState): Option[Seq[Ast]] = {
    if (superCVAst.size != 2) None else {
      val conditionalCodeBlockNode: Node = superCVAst.getNode(1)
      val codeBlocks: Seq[Ast] = conditionalHandler
        .handleAndSimplifyConditionalExtended(conditionalCodeBlockNode, converterState, handleCodeBlock)
      Option(codeBlocks)
    }
  }
  
  /**
   * Defines the Method instruction handler.
   */
  private def handleCodeBlock(codeBlockRootNode: Node, converterState: VAstConverterState): Seq[Ast] = {
    val newConverterState: VAstConverterState = variableHandler.addNewVariableNamespace(converterState)
    
    // Translates the C instructions of the current code block.
    var methodInstructions: Seq[Ast] = Seq.empty[Ast]
    val numberOfInstructions: Int = codeBlockRootNode.size
    for (instructionIndex: Int <- 0 until numberOfInstructions) {
      methodInstructions ++= converter.convert(codeBlockRootNode.getNode(instructionIndex), newConverterState)
    }

    // orders the ASTs by its code position.
    val orderedInstructionAsts: Seq[(Int, Int, Ast)] = methodInstructions.flatMap((ast: Ast) => ast match {
        case subAst if subAst.root.isEmpty => None // If the Ast is empty (does not contain any node).
        case subAst if conditionalHandler.isJoernChoiceNode(subAst.root.get) =>
          // If the AST has a JOERN choice node as root node.
          val rootChoiceNode: NewControlStructure = subAst.root.get.asInstanceOf[NewControlStructure]
          val (line: Int, column: Int) = subAst.edges
            .filter((edge: AstEdge) => edge.src.equals(rootChoiceNode))
            .map((edge: AstEdge) => getLocationInformation(edge.dst))
            .sortWith({case ((line1: Int, column1: Int), (line2: Int, column2: Int)) =>
              (line1 < line2) || ((line1 == line2) && (column1 < column2))})
            .head

          Option((line, column, subAst))
          
        case subAst =>
          // If the Ast contains at least one node.
          val (line: Int, column: Int) = getLocationInformation(subAst.root.get)
          Option((line, column, subAst))
          
      })
      .sortWith({case ((line1: Int, column1: Int, ast1: Ast), (line2: Int, column2: Int, ast2: Ast)) =>
        (line1 < line2) || ((line1 == line2) && (column1 < column2))})
    
    // Determines the code position of the new code block.
    val (line: Option[Int], column: Option[Int]) = if (orderedInstructionAsts.isEmpty) (None, None) else {
      val (linePosition: Int, columnPosition: Int, firstInstructionAst: Ast) = orderedInstructionAsts.head
      (Option(linePosition), Option(columnPosition))
    }

    // Combines the ordered ASTs into one code block.
    val codeBlockNode: NewBlock = vAstCreator.emptyBlockNodeHelper(codeBlockRootNode, line, column)
    var codeBlockAst: Ast = vAstCreator.AstHelper(codeBlockNode)
    var codeParts: Seq[String] = Seq.empty[String]
    for ((line: Int, column: Int, instructionAst: Ast) <- orderedInstructionAsts) {
      val instructionAstRootNode: AstNodeNew = instructionAst.root.get.asInstanceOf[AstNodeNew]
      codeParts ++= Seq(instructionAstRootNode.code)
      if (isVirtualCodeBlock(instructionAst)) {
        // If the AST has a virtual code block node (a code block node without its own variable namespace and is only
        // required by the JOERN AST structure, so it has no code structure in the original C code) as the root node.
        // This virtual code block node is a result of the JOERN AST tree structure and is used to describe variable
        // code instruction sequences and switch-case instruction sequences.

        // Adds the instructions contained in the virtual code block AST to the new code block.
        codeBlockAst = Ast(
          nodes = codeBlockAst.nodes ++ instructionAst.nodes.filterNot((node: NewNode) => node == instructionAstRootNode),
          edges = codeBlockAst.edges ++ instructionAst.edges
            .map((edge: AstEdge) => if (edge.src == instructionAstRootNode) AstEdge(codeBlockNode, edge.dst) else edge),
          conditionEdges = codeBlockAst.conditionEdges ++ instructionAst.conditionEdges
            .map((edge: AstEdge) => if (edge.src == instructionAstRootNode) AstEdge(codeBlockNode, edge.dst) else edge),
          argEdges = codeBlockAst.argEdges ++ instructionAst.argEdges
            .map((edge: AstEdge) => if (edge.src == instructionAstRootNode) AstEdge(codeBlockNode, edge.dst) else edge),
          receiverEdges = codeBlockAst.receiverEdges ++ instructionAst.receiverEdges
            .map((edge: AstEdge) => if (edge.src == instructionAstRootNode) AstEdge(codeBlockNode, edge.dst) else edge),
          refEdges = codeBlockAst.refEdges ++ instructionAst.refEdges
            .map((edge: AstEdge) => if (edge.src == instructionAstRootNode) AstEdge(codeBlockNode, edge.dst) else edge),
          bindsEdges = codeBlockAst.bindsEdges ++ instructionAst.bindsEdges
            .map((edge: AstEdge) => if (edge.src == instructionAstRootNode) AstEdge(codeBlockNode, edge.dst) else edge),
          captureEdges = codeBlockAst.captureEdges ++ instructionAst.captureEdges
            .map((edge: AstEdge) => if (edge.src == instructionAstRootNode) AstEdge(codeBlockNode, edge.dst) else edge)
        )
        
      } else {
        // Adds the instruction contained in the Asts to the new code block.
        codeBlockAst = codeBlockAst.withChild(instructionAst)
      }
    }

    // Creates the new single root block node.
    val code: String = codeParts.mkString("\n")
    val codeBlockCode: String = s"{\n$code\n}"
    codeBlockNode.code(codeBlockCode)

    // Returns the code block.
    Seq(codeBlockAst)
  }
  
  private def getLocationInformation(node: NewNode): (Int, Int) =  {
    val rootNode: AstNodeNew = node.asInstanceOf[AstNodeNew]
    val line: Int = rootNode.lineNumber.getOrElse(Int.MaxValue)
    val column: Int = rootNode.columnNumber.getOrElse(Int.MaxValue)
    (line, column)
  }

  private def isVirtualCodeBlock(subAst: Ast): Boolean = {
    // TODO: Check if the passed AST has a virtual code block as root node.
    false
  }
}
