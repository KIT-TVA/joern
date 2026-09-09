package io.joern.dataflowengineoss.dotgenerator

import io.joern.dataflowengineoss.DefaultSemantics
import io.shiftleft.codepropertygraph.generated.nodes.Method
import io.joern.dataflowengineoss.semanticsloader.Semantics
import io.shiftleft.semanticcpg.dotgenerator.{AstGenerator, CdgGenerator, CfgGenerator, DotSerializer}

object DotCpg14Generator {

  def toDotCpg14(traversal: Iterator[Method], extendedView: Boolean = false, withColoring: Boolean = false,
                 forceTreeStructure: Boolean = false)(implicit semantics: Semantics = DefaultSemantics()): Iterator[String] =
    traversal.map((method: Method) => dotGraphForMethod(method, extendedView, withColoring, forceTreeStructure))

  private def dotGraphForMethod(method: Method, extendedView: Boolean = false, withColoring: Boolean = false,
                                forceTreeStructure: Boolean = false)(implicit semantics: Semantics): String = {
    val ast = new AstGenerator().generate(method)
    val cfg = new CfgGenerator().generate(method)
    val ddg = new DdgGenerator().generate(method)
    val cdg = new CdgGenerator().generate(method)
    DotSerializer.dotGraph(Option(method), ast ++ cfg ++ ddg ++ cdg, withEdgeTypes = true, extendedView=extendedView,
                           withColoring=withColoring, forceTreeStructure=forceTreeStructure)
  }

}
