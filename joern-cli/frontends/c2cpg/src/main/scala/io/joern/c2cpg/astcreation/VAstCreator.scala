package io.joern.c2cpg.astcreation

import io.joern.c2cpg.Config
import io.joern.c2cpg.parser.HeaderFileFinder
import io.joern.x2cpg.Ast
import io.joern.x2cpg.AstCreatorBase
import io.joern.x2cpg.ValidationMode
import io.joern.x2cpg.datastructures.Stack.*
import io.joern.x2cpg.datastructures.VariableScopeManager
import io.shiftleft.codepropertygraph.generated.{Cpg, DiffGraphBuilder, NodeTypes}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal
import org.eclipse.cdt.core.dom.ast.IASTNode
import org.eclipse.cdt.core.dom.ast.IASTTranslationUnit
import org.slf4j.Logger
import org.slf4j.LoggerFactory
import superc.SuperC
import xtc.tree.{GNode, Node}


class VAstCreator (
                    val filename: String,
                    //val global: CGlobal,
                    //val config: Config,
                    val superCAst: Node,
                    //val headerFileFinder: HeaderFileFinder
                  ) extends AstCreatorBase[Node, VAstCreator](filename)(ValidationMode.Disabled){
  protected implicit val schemaValidation: ValidationMode = ValidationMode.Disabled
  protected val scope: VariableScopeManager = new CVariableScopeManager()
  protected val logger: Logger = LoggerFactory.getLogger(classOf[VAstCreator])

  def superC2JoernGraph(joernAST: Int): Cpg = {
    //joernAST.
    //joernAST.getLocation
    //TODO: return actual cpg
    Cpg()
  }

  override def createAst(): DiffGraphBuilder = {
    //TODO: filecontent
    val fileNode    =  NewFile().name("test").order(0)
    val ast = Ast(fileNode).withChild(astForXtcNode(superCAst))
    Ast.storeInDiffGraph(ast, diffGraph)
    scope.createVariableReferenceLinks(diffGraph, filename)
    diffGraph
  }
  def astForXtcNode(node: Node): Ast = {
    val diffGraph: DiffGraphBuilder = Cpg.newDiffGraphBuilder
    val joernNode = convertXTCNodeToJoern(node.getNode(0).getNode(0).getNode(1))
    //implicit val validationMode: ValidationMode = ValidationMode.Disabled
    Ast(joernNode)
  }

  def convertXTCNodeToJoern(node: Node): NewNode = {
    node match {
      case node: GNode if node.hasName("FunctionDefinition") =>
        val functionPrototype: Node = node.getNode(0)
        val returnType = functionPrototype.getNode(0).getString(0)
        val name = functionPrototype.getNode(1).getNode(0).getString(0)
        NewCall()
          .name(name)
          .typeFullName(returnType)
      case _ => NewReturn()
    }
  }


  override protected def line(node: Node): Option[Int] = ???

  override protected def column(node: Node): Option[Int] = ???

  override protected def lineEnd(node: Node): Option[Int] = ???

  override protected def columnEnd(element: Node): Option[Int] = ???

  override protected def code(node: Node): String = ???
}
