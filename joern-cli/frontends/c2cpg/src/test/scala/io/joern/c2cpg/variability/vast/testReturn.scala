package io.joern.c2cpg.variability.vast

import io.joern.c2cpg.astcreation.{CGlobal, VAstCreator}
import io.joern.c2cpg.testfixtures.C2CpgSuite
import io.joern.c2cpg.variability.util.TestUtil.generateVASTDot
import io.joern.dataflowengineoss.DefaultSemantics
import io.joern.dataflowengineoss.dotgenerator.DotDdgGenerator
import io.joern.dataflowengineoss.passes.reachingdef.ReachingDefPass
import io.joern.dataflowengineoss.semanticsloader.Semantics
import io.joern.x2cpg.X2Cpg
import io.joern.x2cpg.X2Cpg.newEmptyCpg
import io.joern.x2cpg.passes.frontend.MetaDataPass
import io.shiftleft.codepropertygraph.generated.{DiffGraphBuilder, Languages, nodes}
import io.shiftleft.semanticcpg.dotgenerator.DotAstGenerator
import superc.SuperC

import java.io.{File, StringReader}

class testReturn extends C2CpgSuite(withOssDataflow = true) {
  val cCode =
    """
      | int foo(int a){
      |   int b = a + 42;
      |   return b;
      | }
      |""".stripMargin
  val cCpg = code(cCode)
  val cTraversal = cCpg.graph._nodes(25).asInstanceOf[Iterator[nodes.Method]]
  val cAstDotString = DotAstGenerator.dotAst(cTraversal)
  println("Standard Joern C AST:")
  println(cAstDotString.mkString)


  val superCAstDotString = generateVASTDot(cCode)

  println("\n\n\nSuperC (V)AST:")
  println(superCAstDotString)


}
