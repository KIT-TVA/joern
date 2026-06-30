package io.joern.c2cpg.astcreation.converter

import io.joern.c2cpg.astcreation.{Defines, VAstCreatorNew}
import io.joern.x2cpg.Ast
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.{NewFieldIdentifier, NewIdentifier}
import xtc.tree.{Location, Node}

/** SuperC: DirectSelection (s.x) and IndirectSelection (p->x). */
class VAstPatternConverterForMemberAccess(vAstCreator: VAstCreatorNew, converter: VAstConverter)
  extends VAstPatternConverter(
    vAstCreator,
    converter,
    List(
      "DirectSelection",
      "IndirectSelection"
    )
  ) {

  override def convert(superCVAst: Node, converterState: VAstConverterState): Option[Seq[Ast]] = {
    if (superCVAst.size() < 2) None
    else {
      val isArrow  = superCVAst.getName == "IndirectSelection"
      val joernOp  = if (isArrow) Operators.indirectFieldAccess else Operators.fieldAccess
      val sep      = if (isArrow) "->" else "."
      val fieldName = memberName(superCVAst, 1)
      if (fieldName.isEmpty) None
      else {
        val ownerAst = operandAst(superCVAst.getNode(0), converterState)
        val (line, column) = locationOf(superCVAst)
        val code = s"${astCode(ownerAst)}$sep$fieldName"
        val call = vAstCreator.callNodeHelper(
          superCVAst,
          code,
          joernOp,
          joernOp,
          DispatchTypes.STATIC_DISPATCH,
          None,
          Option(Defines.Any),
          line,
          column
        )
        val fieldId = NewFieldIdentifier()
          .canonicalName(fieldName)
          .code(fieldName)
          .lineNumber(line)
          .columnNumber(column)
        Option(Seq(vAstCreator.callAst(call, List(ownerAst, vAstCreator.AstHelper(fieldId)))))
      }
    }
  }

  private def memberName(node: Node, index: Int): String =
    node.get(index) match {
      case value: String => value
      case child: Node   => firstStringChild(child)
      case _             => ""
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
