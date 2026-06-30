package io.joern.c2cpg.astcreation.converter

import io.joern.c2cpg.astcreation.{Defines, VAstCreatorNew}
import io.joern.x2cpg.Ast
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.{NewIdentifier, NewLiteral}
import xtc.tree.{Location, Node}

class VAstPatternConverterForUnaryOperators(vAstCreator: VAstCreatorNew, converter: VAstConverter)
  extends VAstPatternConverter(
    vAstCreator,
    converter,
    List(
      "Increment",
      "Decrement",
      "UnaryExpression"
    )
  ) {

  private val OperatorMap: Map[String, String] = Map(
    "+"      -> Operators.plus,
    "-"      -> Operators.minus,
    "*"      -> Operators.indirection,
    "&"      -> Operators.addressOf,
    "~"      -> Operators.not,
    "!"      -> Operators.logicalNot,
    "sizeof" -> Operators.sizeOf,
    "typeof" -> Defines.OperatorTypeOf
  )

  override def convert(superCVAst: Node, converterState: VAstConverterState): Option[Seq[Ast]] = {
    extractUnaryParts(superCVAst) match {
      case None => None
      case Some((operandNode, opString, prefix)) =>
        val joernOp = joernUnaryOp(opString, prefix)
        val operandAst = parameterConverter(operandNode, converterState)
        val (line, column) = locationOf(superCVAst)
        val code =
          if (prefix) s"$opString${astCode(operandAst)}".trim
          else s"${astCode(operandAst)}$opString".trim
        val call = vAstCreator.callNodeHelper(
          superCVAst, code, joernOp, joernOp, DispatchTypes.STATIC_DISPATCH, None, Option(Defines.Any), line, column
        )
        Option(Seq(vAstCreator.callAst(call, List(operandAst))))
    }
  }

  private def isIncrementDecrementToken(op: String): Boolean = op == "++" || op == "--"

  private def isUnaryOperatorToken(op: String): Boolean =
    op.nonEmpty && (OperatorMap.contains(op) || isIncrementDecrementToken(op))

  private def extractUnaryParts(node: Node): Option[(Node, String, Boolean)] = {
    if (node.size() < 2) None
    else {
      val op0 = operatorTokenAt(node, 0)
      val op1 = operatorTokenAt(node, 1)
      (op0, op1) match {
        case (Some(op), None) => childNodeAt(node, 1).map(n => (n, op, true))
        case (None, Some(op)) => childNodeAt(node, 0).map(n => (n, op, false))
        case (Some(op), Some(_)) => childNodeAt(node, 1).map(n => (n, op, true))
        case (None, None)     => operandOperatorLayout(node)
      }
    }
  }

  /** SuperC layout used by VAstCreator: operand at [0], operator token at [1].getString(0). */
  private def operandOperatorLayout(node: Node): Option[(Node, String, Boolean)] = {
    val operand = childNodeAt(node, 0)
    val opToken = operatorTokenFromChild(node, 1)
    (operand, opToken) match {
      case (Some(opNode), Some(op)) => Option((opNode, op, false))
      case _ =>
        val prefixOp = operatorTokenFromChild(node, 0)
        val postfixOperand = childNodeAt(node, 1)
        (prefixOp, postfixOperand) match {
          case (Some(op), Some(opNode)) => Option((opNode, op, true))
          case _                        => None
        }
    }
  }

  private def operatorTokenAt(node: Node, index: Int): Option[String] =
    operatorTokenFromChild(node, index)

  private def operatorTokenFromChild(parent: Node, index: Int): Option[String] = {
    if (index >= parent.size()) None
    else
      parent.get(index) match {
        case value: String if isUnaryOperatorToken(value) => Some(value)
        case child: Node =>
          val token = tokenFromNode(child)
          if (isUnaryOperatorToken(token)) Some(token) else None
        case _ => None
      }
  }

  /** Reads operator text from a SuperC syntax/operator child (matches VAstCreator getString(0)). */
  private def tokenFromNode(node: Node): String = {
    if (node.size() > 0) {
      node.get(0) match {
        case value: String => value
        case _             => safeGetString(node, 0)
      }
    } else {
      safeGetString(node, 0)
    }
  }

  private def safeGetString(node: Node, index: Int): String =
    try {
      val value = node.getString(index)
      if (value == null) "" else value
    } catch {
      case _: Exception =>
        if (node.getName == "superc.core.Syntax$Text") firstStringChild(node) else ""
    }

  private def joernUnaryOp(opString: String, prefix: Boolean): String =
    (opString, prefix) match {
      case ("++", true)  => Operators.preIncrement
      case ("++", false) => Operators.postIncrement
      case ("--", true)  => Operators.preDecrement
      case ("--", false) => Operators.postDecrement
      case (op, _)       => OperatorMap.getOrElse(op, Defines.OperatorUnknown)
    }

  private def childNodeAt(node: Node, index: Int): Option[Node] =
    if (index >= node.size()) None
    else
      node.get(index) match {
        case child: Node => Some(child)
        case _           => None
      }

  private def parameterConverter(node: Node, converterState: VAstConverterState): Ast = {
    val converted = converter.convert(node, converterState)
    if (converted.nonEmpty && converted.head.root.isDefined) converted.head
    else node.getName match {
      case "PrimaryIdentifier"       => identifierAst(node)
      case "superc.core.Syntax$Text" => literalAst(node)
      case _ if node.size() == 1 =>
        node.get(0) match {
          case child: Node => parameterConverter(child, converterState)
          case _           => vAstCreator.AstHelper()
        }
      case _ => vAstCreator.AstHelper()
    }
  }

  private def identifierAst(node: Node): Ast = {
    val name = firstStringChild(node)
    val (line, column) = locationOf(node)
    val id = NewIdentifier()
      .name(name)
      .code(name)
      .typeFullName(Defines.Any)
      .lineNumber(line)
      .columnNumber(column)
    vAstCreator.AstHelper(id)
  }

  private def literalAst(node: Node): Ast = {
    val (line, column) = locationOf(node)
    val lit = NewLiteral().code(firstStringChild(node)).typeFullName(Defines.Any).lineNumber(line).columnNumber(column)
    vAstCreator.AstHelper(lit)
  }

  private def firstStringChild(node: Node): String = {
    var i = 0
    while (i < node.size()) {
      node.get(i) match {
        case value: String => return value
        case child: Node   =>
          val nested = firstStringChild(child)
          if (nested.nonEmpty) return nested
        case _ =>
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
