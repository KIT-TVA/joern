package io.joern.c2cpg.astcreation.converter

import io.joern.c2cpg.astcreation.VAstCreatorNew
import io.joern.c2cpg.astcreation.converter.VAstConverter
import io.joern.x2cpg.Ast
import io.shiftleft.codepropertygraph.generated.nodes.{Method, NewBlock, NewMethod, NewMethodReturn, NewNode}
import xtc.tree.Node

import scala.collection.mutable.ListBuffer

class VAstPatternConverterForSuperCRoot(vAstCreator: VAstCreatorNew, converter: VAstConverter)
  extends VAstPatternConverter(vAstCreator, converter, List.apply("TranslationUnit")) {

  private val globaleFileMethodeName: String = "<global>"
  private val globaleFileMethodeReturnType: String = "ANY"
  private val globaleFileMethodeLine: Int = 1
  private val globaleFileMethodeColumn: Int = 1

  override def convert(superCVAst: Node): Option[Seq[Ast]] = {
    if (superCVAst.size() != 1)
      throw new RuntimeException("The structure of the SuperC VAST root does not match the expected structure.")

    val externalDeclarationListNode: Node = superCVAst.getNode(0)
    if (!externalDeclarationListNode.getName.equals("ExternalDeclarationList"))
      throw new RuntimeException("The structure of the SuperC VAST root does not match the expected structure.")

    // Creates the subtrees of all globale declarations.
    val definedFunctions: ListBuffer[Ast] =  new ListBuffer[Ast]()
    val globalCodeBlockStatements: ListBuffer[Ast] = new ListBuffer[Ast]()
    for (index: Int <- 0 until externalDeclarationListNode.size) {
      val astSubtrees: Seq[Ast] = converter.convert(externalDeclarationListNode.getNode(index))

      if (astSubtrees.isEmpty) {
        val error: String = "At least one element was expected at the top level, but  no elements were returned."
        throw new RuntimeException(error)
      }

      // Adds all new subtrees.
      for (astSubtree: Ast <- astSubtrees)  {
        val astSubtreeRootNode: Option[NewNode] = astSubtree.root
        if (astSubtreeRootNode.isEmpty) throw new RuntimeException("The returned AST does not contain a root node.")

        // Splits the subtrees into functions and other global declarations.
        astSubtreeRootNode.get.getClass.toString match {
          case "class io.shiftleft.codepropertygraph.generated.nodes.NewMethod" => definedFunctions += astSubtree
          case _ => globalCodeBlockStatements += astSubtree
        }
      }
    }

    // Creates the code block with all global declarations.
    val globalCodeBlockBlock: NewBlock = vAstCreator.emptyBlockNodeHelper(externalDeclarationListNode,
                                                                          Option(globaleFileMethodeLine),
                                                                          Option(globaleFileMethodeColumn))
    val globalCodeBlock: Ast = vAstCreator.blockAstHelper(globalCodeBlockBlock, globalCodeBlockStatements.toList)

    // Creates the JOERN specific global function declaration.
    val methodNode = NewMethod()
      .name(globaleFileMethodeName)
      .filename(vAstCreator.getCurrentFilename)
      .code(globaleFileMethodeName)
      .fullName(s"${vAstCreator.getCurrentFilename}:${globaleFileMethodeName}")
      .lineNumber(globaleFileMethodeLine)
      .columnNumber(globaleFileMethodeColumn)
    val returnStatement: NewMethodReturn = vAstCreator.methodReturnNodeHelper(superCVAst, globaleFileMethodeReturnType)
      .lineNumber(globaleFileMethodeLine)
      .columnNumber(globaleFileMethodeColumn)

    val method: Ast = vAstCreator.methodAstHelper(
      methodNode,
      definedFunctions.toList,
      globalCodeBlock,
      returnStatement,
      modifiers = List()
    )
    Option(Seq(method))
  }
}
