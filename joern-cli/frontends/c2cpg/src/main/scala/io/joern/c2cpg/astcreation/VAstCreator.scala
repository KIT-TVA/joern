package io.joern.c2cpg.astcreation

import com.rits.cloning.Cloner
import io.circe.syntax.*
import io.joern.x2cpg.datastructures.VariableScopeManager
import io.joern.x2cpg.{Ast, AstCreatorBase, AstEdge, ValidationMode}
import io.shiftleft.codepropertygraph.generated.*
import io.shiftleft.codepropertygraph.generated.nodes.*
import org.slf4j.{Logger, LoggerFactory}
import superc.core.PresenceConditionManager
import superc.core.PresenceConditionManager.PresenceCondition
import superc.core.Syntax.Text
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
  protected var choiceNodeIdCounter: Int = 0
  protected var presenceConditions: Map[String, PresenceCondition] = Map()

  private val OperatorMap: Map[String, String] = Map(

    ">" -> Operators.greaterThan,
    "*" -> Operators.multiplication,
    "/" -> Operators.division,
    "%" -> Operators.modulo,
    "+" -> Operators.addition,
    "-" -> Operators.subtraction,
    "=" -> Operators.assignment,
    "<" -> Operators.lessThan,
    "<=" -> Operators.lessEqualsThan,
    "=" -> Operators.equals,
    ">=" -> Operators.greaterEqualsThan,


    /*IASTBinaryExpression.op_shiftLeft -> Operators.shiftLeft,
    IASTBinaryExpression.op_shiftRight -> Operators.arithmeticShiftRight,
    IASTBinaryExpression.op_binaryAnd -> Operators.and,
    IASTBinaryExpression.op_binaryXor -> Operators.xor,
    IASTBinaryExpression.op_binaryOr -> Operators.or,
    IASTBinaryExpression.op_logicalAnd -> Operators.logicalAnd,
    IASTBinaryExpression.op_logicalOr -> Operators.logicalOr,
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
    IASTBinaryExpression.op_notequals -> Operators.notEquals,
    IASTBinaryExpression.op_pmdot -> Operators.indirectFieldAccess,
    IASTBinaryExpression.op_pmarrow -> Operators.indirectFieldAccess,
    IASTBinaryExpression.op_max -> Defines.OperatorMax,
    IASTBinaryExpression.op_min -> Defines.OperatorMin,
    IASTBinaryExpression.op_ellipses -> Defines.OperatorEllipses*/
  )


  override def createAst(): DiffGraphBuilder = {
    //TODO: filecontent
    val fileNode = NewFile().name("test").order(0)
    //TODO: remove this
    // val test = astForIf(superCAst.getNode(0).getNode(0).getNode(1).getNode(1).getNode(1).getNode(0).getNode(1))
    val ast = Ast(fileNode).withChild(astForXtcTree(superCAst))
    Ast.storeInDiffGraph(ast, diffGraph)
    //    diffGraph.addEdge()
    scope.createVariableReferenceLinks(diffGraph, filename)
    diffGraph
  }

  def astForXtcTree(node: Node): Ast = {
    val diffGraph: DiffGraphBuilder = Cpg.newDiffGraphBuilder
    //    val testI = convertXTCNodeToJoern(node.getNode(0).getNode(0).getNode(1).getNode(1).getNode(1).getNode(0).getNode(1))
    val joernNode = astForXtcNoce(node.getNode(0).getNode(0).getNode(1))
    //implicit val validationMode: ValidationMode = ValidationMode.Disabled
    //Ast(joernNode)
    joernNode.head
  }

  //TODO: Theoretisch können überall choice nodes sein, auch in params etc.
  def astForXtcNoce(node: Node): Seq[Ast] = {
    /*node match {
      case text: Text[_]
    }*/
    node.getName match {
      case "Conditional" => astForChoiceNode(node)
      case "Declaration" => astForXtcNoce(node.getNode(0)) //TODO ?
      case "DeclaringList" => astsForDeclarationList(node)
      case "SimpleDeclarator" =>
        Seq(astForSimpleDeclaration(node))
      case "CompoundStatement" =>
        Seq(astForBlockStatement(node, blockNode(node)))
      case "FunctionDefinition" => Seq(astForFunctionDefinition(node))
      case "ReturnStatement" =>
        Seq(astForReturnStatement(node))
      case "superc.core.Syntax$Text" => Seq(astForLiteral(node))
      //      case "superc.core.Syntax$Text" if node.getClass == Text[_]  => Seq(astForLiteral(node))
      case "PrimaryIdentifier" => Seq(astForIdentifier(node))
      case "StringLiteralList" =>
        astsForStringLiteralList(node)
      case "RelationalExpression" | "AdditiveExpression" =>
        Seq(astForExpression(node))
      //      case operator: GNode if operator.hasName("superc.core.Syntax$Language") => astForOperator(operator)
      case "SelectionStatement" =>
        node.getNode(0).toString match {
          case "if" => Seq(astForIf(node))
          case _ => Seq(Ast())
        }
      case "ExpressionStatement" =>
        node.getNode(0).getName match {
          case "FunctionCall" => Seq(astForFunctionCall(node))
          case _ => Seq(Ast())
        }
      case node => Seq(Ast()) //getChildren(node).map(convertXTCNodeToJoern).head
    }
  }


  /*  Declaration(
      DeclaringList(
      superc.core.Syntax$Language("int"),

        SimpleDeclarator(superc.core.Syntax$Text("b")),

        AssemblyExpressionOpt(),

        AttributeSpecifierListOpt(),

        InitializerOpt(Initializer(superc.core.Syntax$Text("0")))
      )
    )
    */


  /*  DeclaringList(
      DeclaringList(superc.core.Syntax$Language("int"),

        SimpleDeclarator(superc.core.Syntax$Text("a")),

        AssemblyExpressionOpt(),

        AttributeSpecifierListOpt(),

        InitializerOpt()
      ),

      AttributeSpecifierListOpt(),

      SimpleDeclarator(superc.core.Syntax$Text("x")),

      AssemblyExpressionOpt(),

      AttributeSpecifierListOpt(),

      InitializerOpt(Initializer(superc.core.Syntax$Text("5"))))*/


  /*DeclaringList(superc.core.Syntax$Language("int"),

 ArrayDeclarator(SimpleDeclarator(superc.core.Syntax$Text("a")),

 ArrayAbstractDeclarator(superc.core.Syntax$Text("21"))),

 AssemblyExpressionOpt(),

 AttributeSpecifierListOpt(),

 InitializerOpt())*/

  private def astsForDeclarationList(declaration: Node): Seq[Ast] = {
    // We do not support int x, y = 5;
    // Todo: typedefs, part of class etc
    val childNodes = getChildren(declaration)

    val typeNode = childNodes.head
    val declarator = childNodes(1)
    val assemblyExpressionOpt = childNodes(2)
    val attributeSpecifierListOpt = childNodes(3)
    val initializerOpt = childNodes(4)

    val node = localNode(declarator, declarator.getName, code(declarator), typeNode.toString)

    val declAst = Ast(node)
    var initAst = Ast()
    if (initializerOpt.size() > 0) {
      initAst = astForInitializer(declaration, initializerOpt.getNode(0))
    }
    //TODO:
    //    Seq(Ast(node), initAst)

    Seq(declAst, initAst)
  }

  private def astForSimpleDeclaration(simpleDeclaration: Node): Ast = {
    astForIdentifier(simpleDeclaration)
    //    convertXTCNodeToJoern(simpleDeclaration.getNode(0))
  }

  private def astForInitializer(declarator: Node, init: Node): Ast = {
    init match {
      case equalInit: Node if equalInit.hasName("Initializer") =>
        astForEqualsInitializer(declarator, astForXtcNoce(declarator.getNode(1)).head, astForXtcNoce(equalInit.getNode(0)).head)
      case _ => Ast()
    }
  }

  private def astForEqualsInitializer(declarator: Node, leftAst: Ast, rightAst: Ast): Ast = {
    val assignmentCallNode = callNode(
      declarator,
      code(declarator),
      Operators.assignment,
      Operators.assignment,
      DispatchTypes.STATIC_DISPATCH,
      None,
      Some(Defines.Void)
      //      Some(registerType(Defines.Void))
    )
    callAst(assignmentCallNode, List(leftAst, rightAst))
  }



  /*  private def astForReturnStatement(ret: IASTReturnStatement): Ast = {
    val cpgReturn = returnNode(ret, code(ret))
    nullSafeAst(ret.getReturnValue) match {
      case retAst if retAst.root.isDefined => Ast(cpgReturn).withChild(retAst).withArgEdge(cpgReturn, retAst.root.get)
      case _                               => Ast(cpgReturn)
    }
  }*/

  private def astForReturnStatement(returnStatement: Node): Ast = {
    val cpgReturn = returnNode(returnStatement, code(returnStatement))
    var arg = Ast()
    if (returnStatement.size() > 0) {
      arg = astForXtcNoce(returnStatement.getNode(0)).head
    }
    arg match {
      case retAst if retAst.root.isDefined => Ast(cpgReturn).withChild(retAst).withArgEdge(cpgReturn, retAst.root.get)
      case _ => Ast(cpgReturn)
    }
  }

  private def astForIdentifier(identifier: Node): Ast = {
    //TODO: Im Eclipse Frontend wird noch maybeMethodRefForIdentifier verwendet, nachschauen was bei uns das equivalent ist
    val identifierName = identifier.getNode(0).getString(0)
    val tpe = identifier.getNode(0).getName
    val node = identifierNode(identifier, identifierName, code(identifier), tpe)
    Ast(node)
  }

  private def astForExpression(relExp: Node): Ast = {
    val op = OperatorMap.getOrElse(relExp.getNode(1).getString(0), Defines.OperatorUnknown)
    //TODO registerType?       callNode(relExp, code(relExp), op, op, DispatchTypes.STATIC_DISPATCH, None, Some(registerType(Defines.Any)))
    val callNode_ =
      callNode(relExp, code(relExp), op, op, DispatchTypes.STATIC_DISPATCH, None, Some(Defines.Any))
    val left = astForXtcNoce(relExp.getNode(0)).head
    val right = astForXtcNoce(relExp.getNode(2)).head
    callAst(callNode_, List(left, right))
  }


  private def astsForStringLiteralList(stringLitList: Node): Seq[Ast] = {
    getChildren(stringLitList).flatMap(astForXtcNoce)
    /* if (stringLitList.size == 1) {
       convertXTCNodeToJoern(stringLitList.getNode(0)).head
     }
     else {
       //TODO: Handle this case correctly, how do we even get into this case?
       val literals = getChildren(stringLitList).map(convertXTCNodeToJoern)
       Ast()
       /* Ast(
         nodes = Seq(choiceNode) ++ leftAst.nodes ++ rightAst.nodes,
         edges = leftAst.edges ++ rightAst.edges,
         conditionEdges = leftAst.conditionEdges ++ rightAst.conditionEdges ++ presenceConditionEdges,
         argEdges = leftAst.argEdges ++ rightAst.argEdges,
         receiverEdges = leftAst.receiverEdges ++ rightAst.receiverEdges,
         refEdges = leftAst.refEdges ++ rightAst.refEdges,
         bindsEdges = leftAst.bindsEdges ++ rightAst.bindsEdges,
         captureEdges = leftAst.captureEdges ++ rightAst.captureEdges
       )*/
     }*/
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
    val childAsts = getChildren(blockStmt.getNode(1)).flatMap(astForXtcNoce).toList // blockStmt.getStatements.flatMap(astsForStatement).toList
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

  //TODO: This could be more efficient if we used caching
  override protected def code(node: Node): String = {
    val nodes = collectAllNodes(node)
    nodes.flatMap { n =>

      val nodePrefix = n.getName match {
        case "ReturnStatement" => "return"
        case _ => ""
      }
      (0 until n.size()).map { i =>
        n.get(i) match {
          case code: String => nodePrefix + " " + code
          case _ => nodePrefix 
        }
      }
    }.mkString(" ")
  }


  private def astForLiteral(literal: Node): Ast = {
    literal match {
      case lit: Text[_] => val codeString = code(literal)
        val tpe = lit.tag.toString //TODO: registerType(safeGetType(lit.getExpressionType)) todo:token
        if (codeString == Defines.This) {
          val thisIdentifier = identifierNode(lit, codeString, codeString, tpe)
          // TODO: scope.addVariableReference(codeString, thisIdentifier, tpe, EvaluationStrategies.BY_SHARING)
          Ast(thisIdentifier)
        } else {
          Ast(literalNode(lit, codeString, tpe))
        }
    }

  }

  private def isChoiceNode(node: Node): Boolean = {
    node match {
      case choiceNode: GNode if choiceNode.hasName("Conditional") => true
      case _ => false
    }
  }

  //TODO: check astForCCallExpression for pointerCallAst!
  private def astForFunctionCall(expressionStatement: Node): Ast = {
    val nameNode = expressionStatement.getNode(0).getNode(0)
    val argNodes = getChildren(expressionStatement.getNode(0).getNode(1))
    nameNode match {
      // Joern does not have a specific node for the name of the called function in a function call.
      // SuperC, however, does and allows for these nodes to be choice nodes. In these cases we thus have to move the
      // choice node up in the graph and copy the arguments. We do this in SuperC's graph datastructure, because that
      // is the one used to derive the code for Joern nodes and would otherwise not match.
      case choiceNode: GNode if choiceNode.hasName("Conditional") => {
        val leftNameNode = choiceNode.remove(1)
        expressionStatement.getNode(0).set(0, leftNameNode)
        choiceNode.add(1, expressionStatement)

        if (choiceNode.size == 4) {
          val cloner = new Cloner()
          val clonedExpressionStatement = cloner.deepClone(expressionStatement)
          val rightNameNode = choiceNode.remove(3)
          clonedExpressionStatement.getNode(0).set(0, rightNameNode)
          choiceNode.add(3, clonedExpressionStatement)

        }
        astForXtcNoce(choiceNode).head
      }
      // If the name node is not a choice node we can easily translate it.
      case _ =>
        val name = nameNode.getNode(0).getString(0)
        val dispatchType = DispatchTypes.STATIC_DISPATCH
        val callCpgNode =
          callNode(expressionStatement, code(expressionStatement), name, name, dispatchType, Some(""), Some("registerType(callTypeFullName)"))
        //TODO: Hier ist das Problem, dass die StringLiteralList nochmal potenziell in einem conditional ist, das heißt wir sollten die convert Funktion allgemein verändern!
        val args: Seq[Ast] = getChildren(expressionStatement.getNode(0).getNode(1)).flatMap {
          case stringLiteralList: Node if stringLiteralList.hasName("StringLiteralList") => getChildren(stringLiteralList).flatMap(astForXtcNoce)
          case node: Node => astForXtcNoce(node)
        } //.map(convertXTCNodeToJoern)
        createCallAst(callCpgNode, args)


    }
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
    val blockAst_ : Ast = astForXtcNoce(funcDef.getNode(1)).head
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

  def getPresenceConditionMap: Map[String, PresenceCondition] = {
    presenceConditions
  }

  private def astForIf(ifStmt: Node): Ast = {
    val ifNode = controlStructureNode(ifStmt, ControlStructureTypes.IF, code(ifStmt))
    val condAst = astForXtcNoce(ifStmt.getNode(1)).head
    val thenAst = astForXtcNoce(ifStmt.getNode(2)).head
    val elseAst = ifStmt.size match {
      case 4 => astForXtcNoce(ifStmt.getNode(3)).head
      case _ => Ast()
    }
    controlStructureAst(ifNode, Option(condAst), Seq(thenAst, elseAst))
  }

  //TODO: line number, column number und code + wenn left and right ast equal => einen kann man entfernen?
  // Nein! Man muss beide da behalten, muss in presenceConditionMap gesondert gehandled werdem!
  private def astForChoiceNode(choiceStatement: Node): Seq[Ast] = {

    // This "ID" is not unique and just a workaround, because AstNodes do not have IDs, but we need to identify them
    // to assign presenceCondition properties to them, when they are children of a choice node.
    def calculateAstNodeId(node: NewNode): String = {
      val lineNumber = node.properties("LINE_NUMBER").asInstanceOf[Int]
      val columnNumber = node.properties("COLUMN_NUMBER").asInstanceOf[Int]
      val code = node.properties("CODE").asInstanceOf[String]
      s"$lineNumber, $columnNumber $code"
    }

    val presenceCondition: PresenceCondition = choiceStatement.get(0) match {
      case pc: PresenceCondition => pc
    }
    if (presenceCondition.isTrue || choiceStatement.getNode(1).hasName("PrimaryIdentifier")) {
      astForXtcNoce(choiceStatement.getNode(1))
    }
    else {
      val choiceNode = controlStructureNode(choiceStatement, ControlStructureTypes.CHOICE, code(choiceStatement))
      val leftAst = astForXtcNoce(choiceStatement.getNode(1)).head
      var rightAst = Ast()

      val leftId = calculateAstNodeId(leftAst.root.get)
      var presenceConditionMap: Map[String, String] =
        Map("AST1" -> presenceCondition.toString)
      //        Map(leftId -> presenceCondition.toString)

      var presenceConditionEdges = Seq(AstEdge(choiceNode, leftAst.root.get))
      if (choiceStatement.size() == 4) {
        rightAst = astForXtcNoce(choiceStatement.getNode(3)).head
        //TODO wieder rein kommentiewren: Ast.neighbourValidation(choiceNode, rightAst.root.get, EdgeTypes.AST)
        //        rightAst.root.get.storedRef
        presenceConditionEdges = presenceConditionEdges :+ AstEdge(choiceNode, rightAst.root.get)
        val negatedPresenceCondition = choiceStatement.get(2) match {
          case pc: PresenceCondition => pc
        }
        val rightId = calculateAstNodeId(rightAst.root.get)

        // This should just evaluate to true in the case of
        //                                          Choice
        //                             (macro, node)      (!macro, node)
        // where we can just replace the choice node with one of its child nodes.
        if (leftId == rightId) {
          return Seq(leftAst)
        }
        //        presenceConditionMap =  presenceConditionMap + (rightId-> negatedPresenceCondition.toString)

        presenceConditionMap = presenceConditionMap + ("AST2" -> negatedPresenceCondition.toString)
      }
      else {
        //TODO: rename to fringe?
        presenceConditionMap = presenceConditionMap + ("UNKNOWN" -> presenceCondition.not().toString)
      }
      val presenceConditionMapSerialized = presenceConditionMap.asJson.noSpaces
      choiceNode.presenceCondition(presenceConditionMapSerialized)

      Seq(Ast(
        nodes = Seq(choiceNode) ++ leftAst.nodes ++ rightAst.nodes,
        edges = leftAst.edges ++ rightAst.edges ++ presenceConditionEdges,
        conditionEdges = leftAst.conditionEdges ++ rightAst.conditionEdges, //++ Seq(AstEdge(choiceNode, choiceNode)), //TODO: ++ presenceConditionEdges?
        argEdges = leftAst.argEdges ++ rightAst.argEdges,
        receiverEdges = leftAst.receiverEdges ++ rightAst.receiverEdges,
        refEdges = leftAst.refEdges ++ rightAst.refEdges,
        bindsEdges = leftAst.bindsEdges ++ rightAst.bindsEdges,
        captureEdges = leftAst.captureEdges ++ rightAst.captureEdges
      ))

      //      controlStructureAst(choiceNode, Option(choiceNode), Seq())

      /*val choiceNodeId = choiceNodeIdCounter.toString
      choiceNodeIdCounter += 1
      choiceNode.argumentName(choiceNodeId)
      choiceNode.properties
      choiceNode.presenceCondition(presenceCondition.toString)
      presenceConditions = presenceConditions + (choiceNodeId -> presenceCondition)
      //Entweder argumentname verwenden und gut is oder colum + line  maybe code?
      val leftAst = convertXTCNodeToJoern(conditional.getNode(1))
      var rightAst = Ast()
      var negatedPresenceCondition: Option[PresenceCondition] = None
      if (conditional.size() == 4) {
        rightAst = convertXTCNodeToJoern(conditional.getNode(3))
        /*negatedPresenceCondition = conditional.get(2) match {
          case pc: PresenceCondition => Some(pc)
        }*/
      }


      Ast.neighbourValidation(choiceNode, leftAst.root.get, EdgeTypes.AST)
      var presenceConditionEdges = Seq(AstEdge(choiceNode, leftAst.root.get))
      if (rightAst != Ast()) {
       //TODO wieder rein kommentiewren: Ast.neighbourValidation(choiceNode, rightAst.root.get, EdgeTypes.AST)
        presenceConditionEdges = presenceConditionEdges :+ AstEdge(choiceNode, rightAst.root.get)
      }
      //TODO: add presenceCondition property!
      Ast(
        nodes = Seq(choiceNode) ++ leftAst.nodes ++ rightAst.nodes,
        edges = leftAst.edges ++ rightAst.edges ++ presenceConditionEdges,
        conditionEdges = leftAst.conditionEdges ++ rightAst.conditionEdges ++ presenceConditionEdges,
        argEdges = leftAst.argEdges ++ rightAst.argEdges,
        receiverEdges = leftAst.receiverEdges ++ rightAst.receiverEdges,
        refEdges = leftAst.refEdges ++ rightAst.refEdges,
        bindsEdges = leftAst.bindsEdges ++ rightAst.bindsEdges,
        captureEdges = leftAst.captureEdges ++ rightAst.captureEdges
      )*/

    }

  }
}






