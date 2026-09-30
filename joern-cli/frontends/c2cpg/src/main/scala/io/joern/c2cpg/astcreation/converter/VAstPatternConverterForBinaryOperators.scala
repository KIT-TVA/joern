package io.joern.c2cpg.astcreation.converter

import io.joern.c2cpg.astcreation.{Defines, VAstCreatorNew}
import io.joern.x2cpg.Ast
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.{NewIdentifier, NewLiteral}
import xtc.tree.Node

class VAstPatternConverterForBinaryOperators(vAstCreator: VAstCreatorNew, converter: VAstConverter)
  extends VAstPatternConverter(
    vAstCreator,
    converter,
    List(
      "AssignmentExpression",
      "RelationalExpression",
      "AdditiveExpression",
      "MultiplicativeExpression",
      "ShiftExpression",
      "EqualityExpression",
      "AndExpression",
      "ExclusiveOrExpression",
      "InclusiveOrExpression",
      "LogicalAndExpression",
      "LogicalOrExpression"
    )
  ) {

  private val conditionalHandler = converter.getConditionalHandler

  private val OperatorMap: Map[String, String] = Map(
    "*"   -> Operators.multiplication,
    "/"   -> Operators.division,
    "%"   -> Operators.modulo,
    "+"   -> Operators.addition,
    "-"   -> Operators.subtraction,
    "<<"  -> Operators.shiftLeft,
    ">>"  -> Operators.arithmeticShiftRight,
    "<"   -> Operators.lessThan,
    ">"   -> Operators.greaterThan,
    "<="  -> Operators.lessEqualsThan,
    ">="  -> Operators.greaterEqualsThan,
    "=="  -> Operators.equals,
    "!="  -> Operators.notEquals,
    "&"   -> Operators.and,
    "^"   -> Operators.xor,
    "|"   -> Operators.or,
    "&&"  -> Operators.logicalAnd,
    "||"  -> Operators.logicalOr,
    "="   -> Operators.assignment,
    "*="  -> Operators.assignmentMultiplication,
    "/="  -> Operators.assignmentDivision,
    "%="  -> Operators.assignmentModulo,
    "+="  -> Operators.assignmentPlus,
    "-="  -> Operators.assignmentMinus,
    "<<=" -> Operators.assignmentShiftLeft,
    ">>=" -> Operators.assignmentArithmeticShiftRight,
    "&="  -> Operators.assignmentAnd,
    "^="  -> Operators.assignmentXor,
    "|="  -> Operators.assignmentOr,
    "."   -> Operators.indirectFieldAccess,
    "->"  -> Operators.indirectFieldAccess,
    "max" -> Defines.OperatorMax,
    "min" -> Defines.OperatorMin
  )

  override def convert(superCVAst: Node, converterState: VAstConverterState): Option[Seq[Ast]] = {
    if (superCVAst.size() < 3 && realConditionalIndex(superCVAst).isEmpty) None
    else {
      val asts = convertSlots(superCVAst, childSlots(superCVAst), converterState)
      if (asts.exists(_.root.isDefined)) Option(asts) else None
    }
  }

  /**
   * `#if M1 i < 42 #else i < 10` is one expression whose child is a Conditional.
   * Each branch becomes its own call (`i < 42`, `i < 10`) under a CHOICE.
   * A single call that only keeps the first branch drops the `#else`.
   */
  private def convertSlots(origin: Node, slots: Seq[Any], state: VAstConverterState): Seq[Ast] =
    realConditionalIndex(slots) match {
      case Some(index) =>
        slots(index) match {
          case cond: Node =>
            conditionalHandler.handleConditional(
              cond,
              state,
              (branch, branchState) => convertSlots(origin, slots.updated(index, branch), branchState)
            )
          case _ => foldSlots(origin, slots, state).toSeq
        }
      case None =>
        foldSlots(origin, slots, state).toSeq
    }

  /** Left-associative: `a + b + c` is `(a + b) + c`. The first triple is always one call. */
  private def foldSlots(origin: Node, slots: Seq[Any], state: VAstConverterState): Option[Ast] =
    if (slots.size < 3) None
    else {
      var acc = makeCall(origin, operandAst(slots(0), state), operatorText(slots(1)), operandAst(slots(2), state))
      var i   = 3
      while (i + 1 < slots.size && OperatorMap.contains(operatorText(slots(i)))) {
        acc = makeCall(origin, acc, operatorText(slots(i)), operandAst(slots(i + 1), state))
        i += 2
      }
      Option(acc)
    }

  private def makeCall(origin: Node, leftAst: Ast, opString: String, rightAst: Ast): Ast = {
    val joernOp        = OperatorMap.getOrElse(opString, Defines.OperatorUnknown)
    val (line, column) = locationOf(origin)
    val code           = s"${astCode(leftAst)} $opString ${astCode(rightAst)}".trim
    val typeFullName   = if (joernOp.contains("assignment")) Defines.Void else Defines.Any
    val call = vAstCreator.callNodeHelper(
      origin, code, joernOp, joernOp, DispatchTypes.STATIC_DISPATCH, None, Option(typeFullName), line, column
    )
    vAstCreator.callAst(call, List(leftAst, rightAst))
  }

  private def childSlots(node: Node): Seq[Any] =
    (0 until node.size()).map(i => node.get(i)).toSeq

  private def realConditionalIndex(node: Node): Option[Int] =
    realConditionalIndex(childSlots(node))

  private def realConditionalIndex(slots: Seq[Any]): Option[Int] =
    slots.zipWithIndex.collectFirst {
      case (child: Node, index)
        if conditionalHandler.isSuperCConditionalNode(child) &&
          conditionalHandler.getFirstSuperCCondition(child) != "1" =>
        index
    }

  private def operandAst(slot: Any, state: VAstConverterState): Ast =
    slot match {
      case node: Node => parameterConverter(node, state)
      case _          => vAstCreator.AstHelper()
    }

  private def operatorText(slot: Any): String =
    slot match {
      case value: String => value
      case node: Node    => operatorString(node)
      case _             => ""
    }

  /**
   * Like FunctionCall arguments: `#ifdef` operands must become CHOICE, not a single
   * identifier taken from the first branch (e.g. `ifdef USE_X x #else y` in `x > 0`).
   */
  private def parameterConverter(node: Node, converterState: VAstConverterState): Ast = {
    if (conditionalHandler.isSuperCConditionalNode(node)) {
      if (conditionalHandler.getFirstSuperCCondition(node) == "1") {
        parameterConverter(conditionalHandler.getFirstSuperCConditionalSubtree(node), converterState)
      } else {
        val asts = conditionalHandler.handleConditional(
          node,
          converterState,
          (child, state) => Seq(parameterConverter(child, state))
        )
        asts.find(a => a.root.isDefined).getOrElse(vAstCreator.AstHelper())
      }
    } else {
      val converted = converter.convert(node, converterState)
      if (converted.nonEmpty && converted.head.root.isDefined) converted.head
      else
        node.getName match {
          case "PrimaryIdentifier"       => converter.convert(node, converterState).head
          case "superc.core.Syntax$Text" => literalAst(node)
          case _ if node.size() == 1     => parameterConverter(node.getNode(0), converterState)
          case _                         => vAstCreator.AstHelper()
        }
    }
  }

  private def literalAst(node: Node): Ast = {
    val (line, column) = locationOf(node)
    val lit = NewLiteral().code(firstStringChild(node)).typeFullName(Defines.Any).lineNumber(line).columnNumber(column)
    vAstCreator.AstHelper(lit)
  }

  /**
   * SuperC stores `+=` as an AssignmentOperator whose text is nested (`+` and `=`),
   * not one direct string. Reading only the first string, or defaulting the node to `=`,
   * turns `<operator>.assignmentPlus` into `<operator>.assignment`.
   */
  private def operatorString(operatorNode: Node): String = {
    val pieces = collectOperatorPieces(operatorNode, 0)
    val joined = pieces.mkString
    if (OperatorMap.contains(joined)) joined
    else
      pieces.find(OperatorMap.contains).getOrElse {
        if (operatorNode.getName == "AssignmentOperator") "=" else ""
      }
  }

  private def collectOperatorPieces(node: Node, depth: Int): Seq[String] = {
    if (node == null || depth > 3) Seq.empty
    else {
      val found = scala.collection.mutable.ListBuffer.empty[String]
      var i = 0
      while (i < node.size()) {
        node.get(i) match {
          case value: String if isOperatorPiece(value) => found += value
          case child: Node                             => found ++= collectOperatorPieces(child, depth + 1)
          case _                                       =>
        }
        i += 1
      }
      found.toSeq
    }
  }

  private def isOperatorPiece(value: String): Boolean =
    value.nonEmpty && (OperatorMap.contains(value) || "+-*/%<>=!&|^".exists(ch => value == ch.toString))

  private def firstStringChild(node: Node): String = {
    var i = 0
    while (i < node.size()) {
      node.get(i) match {
        case value: String => return value
        case child: Node =>
          val nested = firstStringChild(child)
          if (nested.nonEmpty) return nested
        case _ =>
      }
      i += 1
    }
    ""
  }

  private def locationOf(node: Node): (Option[Int], Option[Int]) = VAstLiteralLocation.of(node)

  private def astCode(ast: Ast): String =
    ast.root.flatMap(n => codeFromProperty(n.properties.get("CODE"))).getOrElse("")

  private def codeFromProperty(value: Any): Option[String] = value match {
    case null            => None
    case s: String       => Some(s)
    case Some(s: String) => Some(s)
    case Some(other)     => Some(other.toString)
    case other           => Some(other.toString)
  }
}
