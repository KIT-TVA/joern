package io.shiftleft.semanticcpg.dotgenerator

import io.shiftleft.codepropertygraph.generated.nodes.AstNode

object DotAstGenerator {

  private val GLOBAL_DOT_GRAPH_IDENTIFIER: String = "&lt;global&gt;"

  def dotAst[T <: AstNode](traversal: Iterator[T], extended_view: Boolean = false,
                           onlyGlobalGraph: Boolean = false): Iterator[String] = {
    val astDotGraphs: Iterator[String] = traversal.map(e => dotAst(e, extended_view = extended_view))
    if (onlyGlobalGraph) {
      astDotGraphs.filter(dotGraph => dotGraph.startsWith(s"digraph \"$GLOBAL_DOT_GRAPH_IDENTIFIER\" {"))
    } else {
      astDotGraphs
    }
  }

  def dotAst(astRoot: AstNode, extended_view: Boolean): String = {
    val ast = new AstGenerator().generate(astRoot)
    DotSerializer.dotGraph(Option(astRoot), ast, extended_view=extended_view)
  }

  def dotAst(astRoot: AstNode): String = {
    val ast = new AstGenerator().generate(astRoot)
    DotSerializer.dotGraph(Option(astRoot), ast)
  }

}
