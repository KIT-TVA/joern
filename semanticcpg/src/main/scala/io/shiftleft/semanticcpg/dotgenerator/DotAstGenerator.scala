package io.shiftleft.semanticcpg.dotgenerator

import io.shiftleft.codepropertygraph.generated.nodes.AstNode

object DotAstGenerator {

  private val GLOBAL_DOT_GRAPH_IDENTIFIER: String = "&lt;global&gt;"

  def dotAst[T <: AstNode](traversal: Iterator[T], extendedView: Boolean = false,
                           onlyGlobalGraph: Boolean = false, withColoring: Boolean = false): Iterator[String] = {
    val astDotGraphs: Iterator[String] = traversal.map(e => dotAst(e, extendedView=extendedView, withColoring=withColoring))
    if (onlyGlobalGraph) {
      astDotGraphs.filter(dotGraph => dotGraph.startsWith(s"digraph \"$GLOBAL_DOT_GRAPH_IDENTIFIER\" {"))
    } else {
      astDotGraphs
    }
  }

  def dotAst(astRoot: AstNode): String = {
    val ast = new AstGenerator().generate(astRoot)
    DotSerializer.dotGraph(Option(astRoot), ast)
  }

  def dotAst(astRoot: AstNode, withColoring: Boolean): String = {
    dotAst(astRoot, extendedView=false, withColoring=withColoring)
  }

  def dotAst(astRoot: AstNode, extendedView: Boolean, withColoring: Boolean): String = {
    val ast = new AstGenerator().generate(astRoot)
    DotSerializer.dotGraph(Option(astRoot), ast, withEdgeTypes=false, extendedView=extendedView, withColoring=withColoring)
  }
}
