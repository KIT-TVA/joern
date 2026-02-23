package io.joern.c2cpg.variability.evaluation.eval1.desguar_bench.function_parameter_multiple_config

import io.joern.c2cpg.astcreation.{CGlobal, VAstCreator}
import io.joern.c2cpg.testfixtures.C2CpgSuite
import io.joern.c2cpg.variability.util.TestUtil.{generateVASTDot, generateVCPGDot}
import io.joern.dataflowengineoss.DefaultSemantics
import io.joern.dataflowengineoss.dotgenerator.{DotCpg14Generator, DotDdgGenerator, DotPdgGenerator}
import io.joern.dataflowengineoss.passes.reachingdef.ReachingDefPass
import io.joern.dataflowengineoss.semanticsloader.Semantics
import io.joern.x2cpg.X2Cpg
import io.joern.x2cpg.X2Cpg.newEmptyCpg
import io.joern.x2cpg.passes.frontend.MetaDataPass
import io.shiftleft.codepropertygraph.generated.{DiffGraphBuilder, Languages, nodes}
import io.shiftleft.semanticcpg.dotgenerator.{DotAstGenerator, DotCfgGenerator}
import superc.SuperC

import java.io.{File, StringReader}

class zw extends C2CpgSuite(withOssDataflow = true) {
  implicit val semantics: Semantics = DefaultSemantics()
  val cCode =
    """
int foo(
        int z,
        int w)
{
  return 0;
}

int main(int x)
{
  return 0;
}

/*
  foo(w)                  -!A && (!B || B) && (!C || C)
  foo(x,z,w)              -A && B && !C
  foo(y,z,w)              -A && !B && C
  foo(x,y,z,w)            -A && B && C
  foo(z,w)                -A && !B && !C
 */


    """
  val cCpg = code(cCode)
  var cTraversal = cCpg.graph._nodes(25).asInstanceOf[Iterator[nodes.Method]]
  val cAstDotString = DotAstGenerator.dotAst(cTraversal)
  println("Standard Joern C Ast:")
  println(cAstDotString.mkString)

  println()
  println()
  println()
  println()

  cTraversal = cCpg.graph._nodes(25).asInstanceOf[Iterator[nodes.Method]]
  val cCfgDotString = DotCfgGenerator.dotCfg(cTraversal)
  println("Standard Joern C Cfg:")
  println(cCfgDotString.mkString)


  println()
  println()
  println()
  println()

  cTraversal = cCpg.graph._nodes(25).asInstanceOf[Iterator[nodes.Method]]
  val cPdgDotString = DotPdgGenerator.toDotPdg(cTraversal)
  println("Standard Joern C PDG:")
  println(cPdgDotString.mkString)
}
