package io.joern.c2cpg.astcreation.converter

import io.joern.c2cpg.astcreation.VAstCreatorNew
import io.joern.x2cpg.Ast
import xtc.tree.Node

/** Unwraps ( exp ) — Joern has no paren node; pass through inner expression AST. */
class VAstPatternConverterForParenthesizedExpression(vAstCreator: VAstCreatorNew, converter: VAstConverter)
  extends VAstPatternConverter(
    vAstCreator,
    converter,
    List(
      "ParenthesizedExpression"
    )
  ) {

  override def convert(superCVAst: Node, converterState: VAstConverterState): Option[Seq[Ast]] =
    innerNode(superCVAst) match {
      case None => None
      case Some(inner) =>
        val asts = converter.convert(inner, converterState)
        if (asts.nonEmpty) Option(asts) else None
    }

  private def innerNode(node: Node): Option[Node] =
    if (node.size() == 0) None
    else
      node.get(0) match {
        case child: Node => Some(child)
        case _           => try Option(node.getNode(0)) catch { case _: Exception => None }
      }
}
