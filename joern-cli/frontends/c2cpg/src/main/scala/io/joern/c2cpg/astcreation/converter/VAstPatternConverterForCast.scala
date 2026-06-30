package io.joern.c2cpg.astcreation.converter

import io.joern.c2cpg.astcreation.{Defines, VAstCreatorNew}
import io.joern.x2cpg.Ast
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.{NewIdentifier, NewTypeRef}
import xtc.tree.{Location, Node}

/** SuperC: CastExpression / CastExp — type at [0], expression at [1]. Joern: <operator>.cast(TypeRef, expr). */
class VAstPatternConverterForCast(vAstCreator: VAstCreatorNew, converter: VAstConverter)
  extends VAstPatternConverter(
    vAstCreator,
    converter,
    List(
      "CastExpression",
      "CastExp"
    )
  ) {

  override def convert(superCVAst: Node, converterState: VAstConverterState): Option[Seq[Ast]] = {
    castChildren(superCVAst) match {
      case None => None
      case Some((typeNode, exprNode)) =>
        val typeCode = typeText(typeNode)
        val exprAst  = operandAst(exprNode, converterState)
        val code     = s"($typeCode)${astCode(exprAst)}"
        val (line, column) = locationOf(superCVAst)
        val typeRef: NewTypeRef = vAstCreator.typeRefNodeHelper(superCVAst, typeCode, typeCode)
        val call = vAstCreator.callNodeHelper(
          superCVAst,
          code,
          Operators.cast,
          Operators.cast,
          DispatchTypes.STATIC_DISPATCH,
          None,
          Option(typeCode),
          line,
          column
        )
        Option(Seq(vAstCreator.callAst(call, List(vAstCreator.AstHelper(typeRef), exprAst))))
    }
  }

  private def castChildren(node: Node): Option[(Node, Node)] =
    if (node.size() >= 2) Some((childAt(node, 0), childAt(node, 1)))
    else None

  private def childAt(node: Node, index: Int): Node =
    node.get(index) match {
      case child: Node => child
      case _           => node.getNode(index)
    }

  private def typeText(typeNode: Node): String = {
    val text = firstStringChild(typeNode)
    if (text.nonEmpty) text else typeNode.getName
  }

  private def operandAst(node: Node, converterState: VAstConverterState): Ast = {
    val conditionalHandler = converter.getConditionalHandler
    if (conditionalHandler.isConditionalNode(node)) {
      if (conditionalHandler.getFirstCondition(node) == "1") {
        operandAst(conditionalHandler.getFirstConditionalSubtree(node), converterState)
      } else {
        val asts = conditionalHandler.handelConditional(
          node,
          converterState,
          (child, state) => Seq(operandAst(child, state))
        )
        asts.find(_.root.isDefined).getOrElse(vAstCreator.AstHelper())
      }
    } else {
      val converted = converter.convert(node, converterState)
      if (converted.nonEmpty && converted.head.root.isDefined) converted.head
      else {
        node.getName match {
          case "PrimaryIdentifier"                  => identifierAst(node)
          case "superc.core.Syntax$Text"            => identifierAst(node)
          case name if name.contains("Syntax$Text") => identifierAst(node)
          case _ if node.size() >= 1 =>
            node.get(0) match {
              case child: Node => operandAst(child, converterState)
              case _           => vAstCreator.AstHelper()
            }
          case _ => vAstCreator.AstHelper()
        }
      }
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

  private def firstStringChild(node: Node): String = {
    var i = 0
    while (i < node.size()) {
      node.get(i) match {
        case value: String => return value.trim
        case child: Node =>
          val nested = firstStringChild(child)
          if (nested.nonEmpty) return nested
        case _ =>
      }
      i += 1
    }
    try {
      val s = node.getString(0)
      if (s != null && s.nonEmpty) s.trim else ""
    } catch { case _: Exception => "" }
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

