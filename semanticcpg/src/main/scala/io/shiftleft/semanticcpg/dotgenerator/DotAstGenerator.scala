package io.shiftleft.semanticcpg.dotgenerator

import io.shiftleft.codepropertygraph.generated.nodes.AstNode

object DotAstGenerator {

  def dotAst[T <: AstNode](traversal: Iterator[T], extended_view: Boolean = false): Iterator[String] =
    traversal.map(e => dotAst(e, extended_view=extended_view))

  def dotAst(astRoot: AstNode, extended_view: Boolean): String = {
    val ast = new AstGenerator().generate(astRoot)
    DotSerializer.dotGraph(Option(astRoot), ast, extended_view=extended_view)
  }

  def dotAst(astRoot: AstNode): String = {
    val ast = new AstGenerator().generate(astRoot)
    DotSerializer.dotGraph(Option(astRoot), ast)
  }

}
