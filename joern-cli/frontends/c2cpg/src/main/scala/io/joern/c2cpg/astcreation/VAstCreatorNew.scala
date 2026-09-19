package io.joern.c2cpg.astcreation

import io.joern.c2cpg.astcreation.converter.{VAstConverter, VAstConverterState}
import io.joern.x2cpg.datastructures.VariableScopeManager
import io.joern.x2cpg.{Ast, AstCreatorBase, AstNodeBuilder, Defines, ValidationMode}
import io.shiftleft.codepropertygraph.generated.{Cpg, DiffGraphBuilder, PropertyDefaults}
import io.shiftleft.codepropertygraph.generated.nodes.{NewBlock, NewCall, NewControlStructure, NewFile, NewJumpTarget, NewLocal, NewMember, NewMethod, NewMethodReturn, NewModifier, NewNode, NewReturn, NewTypeDecl, NewTypeRef}
import org.slf4j.{Logger, LoggerFactory}
import xtc.tree.{Location, Node}

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
    val astTree: Seq[Ast] =  converter.convert(superCAst, converter.getInitialConverterState)
    //val ast = Ast(fileNode).withChild(astTree.head)
    val ast: Ast = astTree.head
    Ast.storeInDiffGraph(ast, diffGraph)
    scope.createVariableReferenceLinks(diffGraph, filename)
    diffGraph
  }

  def getCurrentFilename: String = {
    filename
  }

  /** Prefer SuperC Location; avoid the old placeholder line/column = 42. */
  private def locationOf(node: Node): (Option[Int], Option[Int]) = {
    val loc: Location = node.getLocation
    if (loc == null) (None, None) else (Option(loc.line), Option(loc.column))
  }

  override protected def line(node: Node): Option[Int] = locationOf(node)._1

  override protected def column(node: Node): Option[Int] = locationOf(node)._2

  override protected def lineEnd(node: Node): Option[Int] = locationOf(node)._1

  override protected def columnEnd(element: Node): Option[Int] = locationOf(element)._2

  override protected def code(node: Node): String = {
    Option(node).map(_.getName).filter(_.nonEmpty).getOrElse("")
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

  def controlStructureNodeHelper(node: Node, controlStructureType: String, code: String,
                                 line: Option[Int] = None, column: Option[Int] = None): NewControlStructure = {
    controlStructureNodeCreator(node, controlStructureType, code, line, column)
  }

  def jumpTargetNodeHelper(
                            node: Node,
                            name: String,
                            code: String,
                            line: Option[Int] = None,
                            column: Option[Int] = None
                          ): NewJumpTarget = {
    NewJumpTarget()
      .parserTypeName(node.getClass.getSimpleName)
      .name(name)
      .code(code)
      .lineNumber(line)
      .columnNumber(column)
  }

  def typeDeclNodeHelper(
                          node: Node,
                          name: String,
                          fullName: String,
                          filename: String,
                          code: String,
                          line: Option[Int] = None,
                          column: Option[Int] = None
                        ): NewTypeDecl = {
    NewTypeDecl()
      .name(name)
      .fullName(fullName)
      .code(code)
      .isExternal(false)
      .filename(filename)
      .lineNumber(line)
      .columnNumber(column)
  }

  def memberNodeHelper(
                        node: Node,
                        name: String,
                        code: String,
                        typeFullName: String,
                        line: Option[Int] = None,
                        column: Option[Int] = None
                      ): NewMember = {
    NewMember()
      .name(name)
      .code(code)
      .typeFullName(typeFullName)
      .lineNumber(line)
      .columnNumber(column)
  }

  def typeRefNodeHelper(node: Node, code: String, typeFullName: String): NewTypeRef =
    typeRefNode(node, code, typeFullName)
}
