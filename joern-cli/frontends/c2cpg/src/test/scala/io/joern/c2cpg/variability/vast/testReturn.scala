package io.joern.c2cpg.variability.vast

import io.joern.c2cpg.astcreation.{CGlobal, VAstCreator}
import io.joern.c2cpg.testfixtures.{C2CpgSuite, CDefaultTestCpg}
import io.joern.c2cpg.variability.util.TestUtil.generateVASTDot
import io.joern.dataflowengineoss.DefaultSemantics
import io.joern.dataflowengineoss.dotgenerator.DotDdgGenerator
import io.joern.dataflowengineoss.passes.reachingdef.ReachingDefPass
import io.joern.dataflowengineoss.semanticsloader.Semantics
import io.joern.x2cpg.X2Cpg
import io.joern.x2cpg.X2Cpg.newEmptyCpg
import io.joern.x2cpg.passes.frontend.MetaDataPass
import io.shiftleft.codepropertygraph.generated.nodes.Method
import io.shiftleft.codepropertygraph.generated.{DiffGraphBuilder, Languages, nodes}
import io.shiftleft.semanticcpg.dotgenerator.DotAstGenerator
import superc.SuperC

import java.io.{File, StringReader}

class testReturn extends C2CpgSuite(withOssDataflow = true) {
  val cCode: String =
    """
      | int foo(int a){
      |   int b = a + 42;
      |   return b;
      | }
      |""".stripMargin
  val cCpg: CDefaultTestCpg = code(cCode)
  val cTraversal: Iterator[Method] = cCpg.graph._nodes(25).asInstanceOf[Iterator[nodes.Method]]
  val cAstDotString: Iterator[String] = DotAstGenerator.dotAst(cTraversal, extended_view=true)
  println("Standard Joern C AST:")
  println(cAstDotString.mkString)


  val (superCAstDotString, superCJoernAstDotString) = generateVASTDot(cCode)
  println("\n\n\nSuperC (V)AST (original data structure):")
  println(superCAstDotString)

  println("\n\n\nSuperC (V)AST (translated to JOERN VAST data structure):")
  println(superCJoernAstDotString)
}
