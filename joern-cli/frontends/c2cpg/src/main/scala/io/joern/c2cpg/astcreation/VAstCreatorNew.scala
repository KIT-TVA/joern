package io.joern.c2cpg.astcreation

import io.joern.c2cpg.astcreation.converter.VAstConverter
import io.joern.x2cpg.datastructures.VariableScopeManager
import io.joern.x2cpg.{Ast, AstCreatorBase, AstNodeBuilder, Defines, ValidationMode}
import io.shiftleft.codepropertygraph.generated.{Cpg, DiffGraphBuilder, PropertyDefaults}
import io.shiftleft.codepropertygraph.generated.nodes.{NewBlock, NewCall, NewControlStructure, NewFile, NewLocal, NewMethod, NewMethodReturn, NewModifier, NewNode, NewReturn}
import org.slf4j.{Logger, LoggerFactory}
import xtc.tree.Node

class VAstCreatorNew(
                     val filename: String,
                     val global: CGlobal,
                     //val config: Config,
                     val superCAst: Node,
                     //val headerFileFinder: HeaderFileFinder
                   ) extends AstCreatorBase[Node, VAstCreatorNew](filename)(ValidationMode.Disabled) {

  protected implicit val schemaValidation: ValidationMode = ValidationMode.Disabled
  protected val scope: VariableScopeManager = new CVariableScopeManager()
  protected val logger: Logger = LoggerFactory.getLogger(classOf[VAstCreator])
  protected val converter: VAstConverter = new VAstConverterForC(this)

  override def createAst(): DiffGraphBuilder = {
    //val fileNode = NewFile().name(filename).order(0)

    val diffGraph: DiffGraphBuilder = Cpg.newDiffGraphBuilder
    val astTree: Seq[Ast] =  converter.convert(superCAst)
    //val ast = Ast(fileNode).withChild(astTree.head)
    val ast: Ast = astTree.head
    Ast.storeInDiffGraph(ast, diffGraph)
    scope.createVariableReferenceLinks(diffGraph, filename)
    diffGraph
  }

  def getCurrentFilename: String = {
    filename
  }

  override protected def line(node: Node): Option[Int] = {
    print(s"requested line for ${node.getName}")
    Option(42)
  }

  override protected def column(node: Node): Option[Int] = {
    print(s"requested column for ${node.getName}")
    Option(42)
  }

  override protected def lineEnd(node: Node): Option[Int] = {
    print(s"requestet lineEnd for ${node.getName}")
    Option(42)
  }

  override protected def columnEnd(element: Node): Option[Int] = {
    print(s"requested columnEnd for ${element.getName}")
    Option(42)
  }

  override protected def code(node: Node): String = {
    print(s"requested code for ${node.getName}")
    "42code42"
  }

  def AstHelper(): Ast = Ast()

  def AstHelper(node: NewNode): Ast = Ast(node)

  def localNodeHelper(node: Node,
                      name: String,
                      code: String,
                      typeFullName: String,
                      closureBindingId: Option[String] = None,
                      genericSignature: Option[String] = None,
                      line: Option[Int] = None,
                      column: Option[Int] = None
                     ): NewLocal = {
    localNodeCreator(node, name, code, typeFullName, closureBindingId, genericSignature, line, column)
  }
  
  def controlStructureHelper(node: Node,
                             controlStructureType: String,
                             code: String,
                             line: Option[Int] = None,
                             column: Option[Int] = None
                            ): NewControlStructure = {
    controlStructureNodeCreator(node, controlStructureType, code, line, column)
  }

  def methodAstHelper(method: NewMethod,
                      parameters: Seq[Ast],
                      body: Ast,
                      methodReturn: NewMethodReturn,
                      modifiers: Seq[NewModifier] = Nil): Ast = {
    methodAst(method, parameters, body, methodReturn, modifiers)
  }

  def methodAstWithAnnotationsHHelper(method: NewMethod,
                                      parameters: Seq[Ast],
                                      body: Ast,
                                      methodReturn: NewMethodReturn,
                                      modifiers: Seq[NewModifier] = Nil,
                                      annotations: Seq[Ast] = Nil
                                     ): Ast = {
    methodAstWithAnnotations(method, parameters, body, methodReturn, modifiers, annotations)
  }

  def emptyBlockNodeHelper(node: Node, line: Option[Int] = None, column: Option[Int] = None): NewBlock = {
    emptyBlockNodeCreator(node, line, column)
  }

  def blockNodeHelper(node: Node,
                      code: String,
                      typeFullName: String,
                      line: Option[Int] = None,
                      column: Option[Int] = None
                     ): NewBlock = {
    blockNodeCreator(node, code, typeFullName, line, column)
  }

  def blockAstHelper(blockNode: NewBlock, statements: List[Ast] = List()): Ast = {
    Ast(blockNode).withChildren(statements)
  }

  def methodReturnNodeHelper(node: Node,
                                       typeFullName: String,
                                       dynamicTypeHintFullName: Option[String] = None,
                                       line: Option[Int] = None,
                                       column: Option[Int] = None
                                       ): NewMethodReturn = {
    methodReturnNodeCreator(node, typeFullName, dynamicTypeHintFullName, line, column)
  }


  def returnNodeHelper(node: Node, code: String,
                                  line: Option[Int] = None, column: Option[Int] = None): NewReturn = {
    returnNodeCreator(node, code, line, column)
  }

  def callNodeHelper(
    node: Node,
    code: String,
    name: String,
    methodFullName: String,
    dispatchType: String,
    signature: Option[String],
    typeFullName: Option[String],
    line: Option[Int] = None,
    column: Option[Int] = None
  ): NewCall = {
    callNodeCreator(node, code, name, methodFullName, dispatchType, signature, typeFullName, line, column)
  }
}
