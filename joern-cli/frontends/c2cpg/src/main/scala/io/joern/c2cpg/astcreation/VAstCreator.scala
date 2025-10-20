package io.joern.c2cpg.astcreation

import io.joern.x2cpg.datastructures.VariableScopeManager
import io.joern.x2cpg.{Ast, AstCreatorBase, AstEdge, ValidationMode}
import io.shiftleft.codepropertygraph.generated.*
import io.shiftleft.codepropertygraph.generated.nodes.*
import org.slf4j.{Logger, LoggerFactory}
import superc.core.PresenceConditionManager
import superc.core.PresenceConditionManager.PresenceCondition
import xtc.tree.{GNode, Location, Node}

import scala.collection.mutable.ListBuffer


class VAstCreator(
                   val filename: String,
                   val global: CGlobal,
                   //val config: Config,
                   val superCAst: Node,
                   //val headerFileFinder: HeaderFileFinder
                 ) extends AstCreatorBase[Node, VAstCreator](filename)(ValidationMode.Disabled)
  with VAstCreatorHelper {
  protected implicit val schemaValidation: ValidationMode = ValidationMode.Disabled
  protected val scope: VariableScopeManager = new CVariableScopeManager()
  protected val logger: Logger = LoggerFactory.getLogger(classOf[VAstCreator])


  private val OperatorMap: Map[String, String] = Map(

    ">" -> Operators.greaterThan,
    /*IASTBinaryExpression.op_multiply -> Operators.multiplication,
    IASTBinaryExpression.op_divide -> Operators.division,
    IASTBinaryExpression.op_modulo -> Operators.modulo,
    IASTBinaryExpression.op_plus -> Operators.addition,
    IASTBinaryExpression.op_minus -> Operators.subtraction,
    IASTBinaryExpression.op_shiftLeft -> Operators.shiftLeft,
    IASTBinaryExpression.op_shiftRight -> Operators.arithmeticShiftRight,
    IASTBinaryExpression.op_lessThan -> Operators.lessThan,
    IASTBinaryExpression.op_lessEqual -> Operators.lessEqualsThan,
    IASTBinaryExpression.op_greaterEqual -> Operators.greaterEqualsThan,
    IASTBinaryExpression.op_binaryAnd -> Operators.and,
    IASTBinaryExpression.op_binaryXor -> Operators.xor,
    IASTBinaryExpression.op_binaryOr -> Operators.or,
    IASTBinaryExpression.op_logicalAnd -> Operators.logicalAnd,
    IASTBinaryExpression.op_logicalOr -> Operators.logicalOr,
    IASTBinaryExpression.op_assign -> Operators.assignment,
    IASTBinaryExpression.op_multiplyAssign -> Operators.assignmentMultiplication,
    IASTBinaryExpression.op_divideAssign -> Operators.assignmentDivision,
    IASTBinaryExpression.op_moduloAssign -> Operators.assignmentModulo,
    IASTBinaryExpression.op_plusAssign -> Operators.assignmentPlus,
    IASTBinaryExpression.op_minusAssign -> Operators.assignmentMinus,
    IASTBinaryExpression.op_shiftLeftAssign -> Operators.assignmentShiftLeft,
    IASTBinaryExpression.op_shiftRightAssign -> Operators.assignmentArithmeticShiftRight,
    IASTBinaryExpression.op_binaryAndAssign -> Operators.assignmentAnd,
    IASTBinaryExpression.op_binaryXorAssign -> Operators.assignmentXor,
    IASTBinaryExpression.op_binaryOrAssign -> Operators.assignmentOr,
    IASTBinaryExpression.op_equals -> Operators.equals,
    IASTBinaryExpression.op_notequals -> Operators.notEquals,
    IASTBinaryExpression.op_pmdot -> Operators.indirectFieldAccess,
    IASTBinaryExpression.op_pmarrow -> Operators.indirectFieldAccess,
    IASTBinaryExpression.op_max -> Defines.OperatorMax,
    IASTBinaryExpression.op_min -> Defines.OperatorMin,
    IASTBinaryExpression.op_ellipses -> Defines.OperatorEllipses*/
  )


  def superC2JoernGraph(joernAST: Int): Cpg = {
    //joernAST.
    //joernAST.getLocation
    //TODO: return actual cpg
    Cpg()
  }

  override def createAst(): DiffGraphBuilder = {
    //TODO: filecontent
    val fileNode = NewFile().name("test").order(0)
    //TODO: remove this
    // val test = astForIf(superCAst.getNode(0).getNode(0).getNode(1).getNode(1).getNode(1).getNode(0).getNode(1))
    val ast = Ast(fileNode).withChild(astForXtcNode(superCAst))
    Ast.storeInDiffGraph(ast, diffGraph)
    scope.createVariableReferenceLinks(diffGraph, filename)
    diffGraph
  }

  def astForXtcNode(node: Node): Ast = {
    val diffGraph: DiffGraphBuilder = Cpg.newDiffGraphBuilder
    //    val testI = convertXTCNodeToJoern(node.getNode(0).getNode(0).getNode(1).getNode(1).getNode(1).getNode(0).getNode(1))
    val joernNode = convertXTCNodeToJoern(node.getNode(0).getNode(0).getNode(1))
    //implicit val validationMode: ValidationMode = ValidationMode.Disabled
    //Ast(joernNode)
    joernNode
  }

  //TODO: Theoretisch können überall choice nodes sein, auch in params etc.
  def convertXTCNodeToJoern(node: Node): Ast = {
    node match {
      case conditional: GNode if conditional.hasName("Conditional") => astForChoiceNode(conditional)
      case compoundStatement: GNode if compoundStatement.hasName("CompoundStatement") =>
        astForBlockStatement(compoundStatement, blockNode(compoundStatement))
      case funcDef: GNode if funcDef.hasName("FunctionDefinition") => astForFunctionDefinition(funcDef)
      case text: GNode if text.hasName("superc.core.Syntax$Text") => astForLiteral(text)
      case identifier: GNode if identifier.hasName("PrimaryIdentifier") => astForIdentifier(identifier)
      case relExpression: GNode if relExpression.hasName("RelationalExpression") => astForRelationalExpression(relExpression)
//      case operator: GNode if operator.hasName("superc.core.Syntax$Language") => astForOperator(operator)
      case selectionStatement: GNode if selectionStatement.hasName("SelectionStatement") =>
        selectionStatement.getNode(0).toString match {
          case "if" => astForIf(selectionStatement)
          case _ => Ast()
        }
      case expressionStatement: GNode if expressionStatement.hasName("ExpressionStatement") =>
        expressionStatement.getNode(0).getName match {
          case "FunctionCall" => astForFunctionCall(expressionStatement)
          case _ => Ast()
        }
      case node => Ast() //getChildren(node).map(convertXTCNodeToJoern).head
    }
  }

  private def astForIdentifier(identifier: Node): Ast = {
    //TODO: Im Eclipse Frontend wird noch maybeMethodRefForIdentifier verwendet, nachschauen was bei uns das equivalent ist
    val identifierName = identifier.getNode(0).getString(0)
    val tpe = identifier.getNode(0).getName
    val node = identifierNode(identifier, identifierName, code(identifier), tpe)
    Ast(node)
  }

  private def astForRelationalExpression(relExp: Node): Ast = {
    val op = OperatorMap.getOrElse(relExp.getNode(1).getString(0), Defines.OperatorUnknown)
    //TODO registerType?       callNode(relExp, code(relExp), op, op, DispatchTypes.STATIC_DISPATCH, None, Some(registerType(Defines.Any)))
    val callNode_ =
      callNode(relExp, code(relExp), op, op, DispatchTypes.STATIC_DISPATCH, None, Some(Defines.Any))
    val left  = convertXTCNodeToJoern(relExp.getNode(0))
    val right = convertXTCNodeToJoern(relExp.getNode(2))
    callAst(callNode_, List(left, right))
  }

  // CompoundStatement(LocalLabelDeclarationListOpt(), DeclarationOrStatementList(...))
  private def astForBlockStatement(blockStmt: Node, blockNode: NewBlock): Ast = {
    val codeString = code(blockStmt)
    val blockLine = line(blockStmt)
    val blockColumn = column(blockStmt)
    val node = blockNode
      .code(codeString)
      .lineNumber(blockLine)
      .columnNumber(blockColumn)
    //.typeFullName(registerType(Defines.Void))
    //scope.pushNewBlockScope(node)
    val childAsts = getChildren(blockStmt.getNode(1)).map(convertXTCNodeToJoern).toList // blockStmt.getStatements.flatMap(astsForStatement).toList
    //    scope.popScope()
    blockAst(node, childAsts)
  }

  /*   ExpressionStatement(FunctionCall(PrimaryIdentifier(superc.core.Syntax$Text("printf")),

   ExpressionList(Conditional(1,

   StringLiteralList(Conditional(1,

   superc.core.Syntax$Text("\"B\"")))))))*/

  def getJoernParam(parameterNode: Node, parentNode: Node, index: Int): NewMethodParameterIn = {
    val returnType = parameterNode.getString(0)
    val name = parameterNode.getNode(1).getString(0)
    val code = returnType + " " + name
    //TODO: was macht child 3, die AttributeSpecifierListOpt()?
    parameterInNode(parentNode, name, code, index, false, "BY_VALUE", returnType)
  }

  //TODO: Think about where Gnode and where Node
  def getChildren(node: Node): Seq[Node] = {
    (0 until node.size()).map(node.get).collect { case g: Node => g }
  }

  override protected def line(node: Node): Option[Int] = getLocations(collectAllNodes(node)) match {
    case Some(locations) => Some(locations._1.line)
    case None => None
  }

  override protected def column(node: Node): Option[Int] = getLocations(collectAllNodes(node)) match {
    case Some(locations) => Some(locations._1.column)
    case None => None
  }

  override protected def lineEnd(node: Node): Option[Int] = getLocations(collectAllNodes(node)) match {
    case Some(locations) => Some(locations._2.line)
    case None => None
  }

  /*private def astForChoice(leftConditional: Node, rightConditional: Option[Node]): Ast = {
    val choiceNode = controlStructureNode(ifStmt, ControlStructureTypes.IF, code(ifStmt))
    Ast()
  }*/


  /*
  def astForBlockStatement(blockStmt: Node, blockNode: NewBlock): Ast = {
    val blockLine = line(blockStmt)
    val blockColumn = column(blockStmt)
    val node = blockNode
      .code(codeString)
      .lineNumber(blockLine)
      .columnNumber(blockColumn)
      .typeFullName(registerType(Defines.Void))
    scope.pushNewBlockScope(node)
    val childAsts = blockStmt.getStatements.flatMap(astsForStatement).toList
    scope.popScope()
    blockAst(node, childAsts)
    val blockLine = line(blockStmt)
    val blockColumn = column(blockStmt)
    val node = blockNode
      .code(codeString)
      .lineNumber(blockLine)
      .columnNumber(blockColumn)
      .typeFullName(registerType(Defines.Void))
    scope.pushNewBlockScope(node)
    val childAsts = blockStmt.getStatements.flatMap(astsForStatement).toList
    scope.popScope()
    blockAst(node, childAsts)
  }*/

  //TODO: Wenn Functions hier immer in einem compund Statement sind, dann kann man die Funktion auch entfernen, wenn nicht
  // muss man hier noch pattern matchen, wie es mit dem IAST C Parser gemacht werden muss
  //def astForMethodBody(bodyCompoundStatement: Node, blockNode: NewBlock): Ast = {
  //  astForBlockStatement(bodyCompoundStatement, blockNode)
  //}

  override protected def columnEnd(element: Node): Option[Int] = getLocations(collectAllNodes(element)) match {
    case Some(locations) => Some(locations._2.column)
    case None => None
  }

  def collectAllNodes(root: Node): List[Node] = {
    val buf = ListBuffer.empty[Node]

    def rec(n: Node): Unit = {
      n.getLocation
      buf += n
      var i = 0
      val s = n.size()
      while (i < s) {
        //TODO: maybe also check lists!
        n.get(i) match {
          case child: Node => rec(child)
          case _ => // ignorieren (String, Int, etc.)

        }
        i += 1
      }
    }

    rec(root)
    buf.toList
  }

  def getLocations(nodes: Seq[Node]): Option[(Location, Location)] = {
    val locations = nodes.filter(_.hasLocation).map(_.getLocation)
    if (!locations.isEmpty) {
      val ord = Ordering.by[Location, (Int, Int)](l => (l.line, l.column))
      Some((locations.min(ord), locations.max(ord)))
    }
    else {
      None
    }

  }

  override protected def code(node: Node): String = {
    val nodes = collectAllNodes(node)
    nodes.flatMap { n =>

      (0 until n.size()).map { i =>
        n.get(i) match {
          case code: String => Some(code)
          case _ => None
        }
      }
    }.collect { case Some(code): Some[String] => code + " "}.mkString
    //TODO: Decide wether we need this implementation, since it has pretty bad performance
  }




  private def astForLiteral(literal: Node): Ast = {
    val codeString = code(literal)
    val tpe = literal.getName //TODO: registerType(safeGetType(lit.getExpressionType))
    if (codeString == Defines.This) {
      val thisIdentifier = identifierNode(literal, codeString, codeString, tpe)
      // TODO: scope.addVariableReference(codeString, thisIdentifier, tpe, EvaluationStrategies.BY_SHARING)
      Ast(thisIdentifier)
    } else {
      Ast(literalNode(literal, codeString, tpe))
    }
  }

  //TODO: check astForCCallExpression for pointerCallAst!
  private def astForFunctionCall(expressionStatement: Node): Ast = {
    val name = expressionStatement.getNode(0).getNode(0).toString
    //TODO: ist das richtig?
    val dispatchType = DispatchTypes.STATIC_DISPATCH
    val callCpgNode =
      callNode(expressionStatement, code(expressionStatement), name, name, dispatchType, Some(""), Some("registerType(callTypeFullName)"))
    val args = getChildren(expressionStatement.getNode(0).getNode(1)).map(convertXTCNodeToJoern)
    createCallAst(callCpgNode, args)
  }

  private def astForFunctionDefinition(funcDef: Node): Ast = {
    val filename = getLocations(collectAllNodes(funcDef)) match {
      case Some((begin, end)) => begin.file
      case None => "noFileFound.txt" //TODO: error
    }
    val functionPrototype: Node = funcDef.getNode(0)
    val returnType = functionPrototype.getNode(0).getString(0)

    val name = functionPrototype.getNode(1).getNode(0).getString(0)
    //TODO: isConstructor etc code...
    //TODO: COde machen wir indem wir in order über die child nodes drüber gehen und da jeweils code() aufrufen (aber diese code funktion müssen wir auch noch schreiben)
    //    val methodBlockNode_ = NewBlock()
    //val blockAst_ : Ast = blockAst(methodBlockNode_, List(): List[Ast])
    val blockAst_ : Ast = convertXTCNodeToJoern(funcDef.getNode(1))
    val methodBlockNode = blockNode(funcDef)
    val methodNode_ = NewMethod()
      .name(name)
      .filename(filename)
    val parameters = functionPrototype.getNode(1).getNode(1).getNode(0) match {
      case ParameterTypeListOpt: Node if ParameterTypeListOpt.size > 0 =>
        val parameterList = ParameterTypeListOpt.getNode(0).getNode(0)

        //TODO: Hier werden conditionals noch ignoriert! (Das mappen nimmt sich einfach den Parameter aus der cond)
        val xtcParameterNodes = getChildren(parameterList).map(_.getNode(1))
        xtcParameterNodes.zipWithIndex.map((paramNode, index) => getJoernParam(paramNode, funcDef, index))
      case _ => Seq()
    }
    // funcDef.getNode(1) is the function body compoundStatement
    //val methodBodyAst = astForMethodBody(funcDef.getNode(1), methodBlockNode)


    methodAst(
      methodNode_,
      parameters.map(Ast(_)),
      blockAst_,
      methodReturnNode(funcDef, returnType),
      modifiers = List()
    )
  }

  private def astForIf(ifStmt: Node): Ast = {
    val ifNode = controlStructureNode(ifStmt, ControlStructureTypes.IF, code(ifStmt))
    val condAst = convertXTCNodeToJoern(ifStmt.getNode(1))
    val thenAst = convertXTCNodeToJoern(ifStmt.getNode(2))
    val elseAst = ifStmt.size match {
      case 4 => convertXTCNodeToJoern(ifStmt.getNode(3))
      case _ => Ast()
    }
    controlStructureAst(ifNode, Option(condAst), Seq(thenAst, elseAst))
  }

  private def astForChoiceNode(conditional: Node): Ast = {
    val presenceCondition: PresenceCondition = conditional.get(0) match {
      case pc: PresenceCondition => pc
    }
    if (presenceCondition.isTrue) {
      convertXTCNodeToJoern(conditional.getNode(1))
    }
    else {
      val choiceNode = controlStructureNode(conditional, ControlStructureTypes.CHOICE, code(conditional))

      val leftAst = convertXTCNodeToJoern(conditional.getNode(1))
      var rightAst = Ast()
      if (conditional.size() == 4) {
        rightAst = convertXTCNodeToJoern(conditional.getNode(3))
      }


      Ast.neighbourValidation(choiceNode, leftAst.root.get, EdgeTypes.AST)
      var presenceConditionEdges = Seq(AstEdge(choiceNode, leftAst.root.get))
      if (leftAst != Ast()) {
        Ast.neighbourValidation(choiceNode, rightAst.root.get, EdgeTypes.AST)
        presenceConditionEdges = presenceConditionEdges :+ AstEdge(choiceNode, rightAst.root.get)
      }
      //TODO: add presenceCondition property!
      Ast(
        nodes = Seq(choiceNode) ++ leftAst.nodes ++ rightAst.nodes,
        edges = leftAst.edges ++ rightAst.edges,
        conditionEdges = leftAst.conditionEdges ++ rightAst.conditionEdges ++ presenceConditionEdges,
        argEdges = leftAst.argEdges ++ rightAst.argEdges,
        receiverEdges = leftAst.receiverEdges ++ rightAst.receiverEdges,
        refEdges = leftAst.refEdges ++ rightAst.refEdges,
        bindsEdges = leftAst.bindsEdges ++ rightAst.bindsEdges,
        captureEdges = leftAst.captureEdges ++ rightAst.captureEdges
      )

    }

  }
}