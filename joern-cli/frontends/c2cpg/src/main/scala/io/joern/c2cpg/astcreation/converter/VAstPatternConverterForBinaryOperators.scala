package io.joern.c2cpg.astcreation.converter

import io.joern.c2cpg.astcreation.{Defines, VAstCreatorNew}
import io.joern.x2cpg.Ast
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.{NewIdentifier, NewLiteral}
import xtc.tree.{Location, Node}

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
    if (superCVAst.size() < 3) None
    else {
      val opString = operatorString(superCVAst.getNode(1))
      val joernOp  = OperatorMap.getOrElse(opString, Defines.OperatorUnknown)
      val leftAst  = parameterConverter(superCVAst.getNode(0), converterState)
      val rightAst = parameterConverter(superCVAst.getNode(2), converterState)
      val (line, column) = locationOf(superCVAst)
      val code = s"${astCode(leftAst)} $opString ${astCode(rightAst)}".trim
      val typeFullName = if (joernOp.contains("assignment")) Defines.Void else Defines.Any
      val call = vAstCreator.callNodeHelper(
        superCVAst, code, joernOp, joernOp, DispatchTypes.STATIC_DISPATCH, None, Option(typeFullName), line, column
      )
      Option(Seq(vAstCreator.callAst(call, List(leftAst, rightAst))))
    }
  }

  private def parameterConverter(node: Node, converterState: VAstConverterState): Ast = {
    val converted = converter.convert(node, converterState)
    if (converted.nonEmpty && converted.head.root.isDefined) converted.head
    else node.getName match {
      case "PrimaryIdentifier"       => identifierAst(node)
      case "superc.core.Syntax$Text" => literalAst(node)
      case _ if node.size() == 1     => parameterConverter(node.getNode(0), converterState)
      case _                         => vAstCreator.AstHelper()
    }
  }

  private def identifierAst(node: Node): Ast = {
    val nameNode = node.getNode(0)
    val (line, column) = locationOf(node)
    val id = NewIdentifier()
      .name(nameNode.getString(0))
      .code(nameNode.getString(0))
      .typeFullName(nameNode.getName)
      .lineNumber(line)
      .columnNumber(column)
    vAstCreator.AstHelper(id)
  }

  private def literalAst(node: Node): Ast = {
    val (line, column) = locationOf(node)
    val lit = NewLiteral().code(firstStringChild(node)).typeFullName(Defines.Any).lineNumber(line).columnNumber(column)
    vAstCreator.AstHelper(lit)
  }

  private def operatorString(operatorNode: Node): String = {
    val fromNode = firstStringChild(operatorNode)
    if (fromNode.nonEmpty) fromNode
    else if (operatorNode.getName == "AssignmentOperator") "="
    else ""
  }

  private def firstStringChild(node: Node): String = {
    var i = 0
    while (i < node.size()) {
      node.get(i) match {
        case value: String => return value
        case _             =>
      }
      i += 1
    }
    ""
  }

  private def locationOf(node: Node): (Option[Int], Option[Int]) = {
    val loc: Location = node.getLocation
    if (loc == null) (None, None) else (Option(loc.line), Option(loc.column))
  }

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
