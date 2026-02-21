package io.joern.c2cpg.variability.evaluation.eval1.desguar_bench.function_parameter_multiple_config

import io.joern.c2cpg.astcreation.{CGlobal, VAstCreator}
import io.joern.c2cpg.testfixtures.C2CpgSuite
import io.joern.c2cpg.variability.util.TestUtil.*
import io.joern.dataflowengineoss.DefaultSemantics
import io.joern.dataflowengineoss.dotgenerator.{DotCpg14Generator, DotDdgGenerator}
import io.joern.dataflowengineoss.passes.reachingdef.ReachingDefPass
import io.joern.dataflowengineoss.semanticsloader.Semantics
import io.joern.x2cpg.X2Cpg
import io.joern.x2cpg.X2Cpg.newEmptyCpg
import io.joern.x2cpg.passes.frontend.MetaDataPass
import io.shiftleft.codepropertygraph.generated.{DiffGraphBuilder, Languages, nodes}
import io.shiftleft.semanticcpg.dotgenerator.DotAstGenerator
import superc.SuperC

import java.io.{File, StringReader}

class Variable extends C2CpgSuite(withOssDataflow = true) {
  implicit val semantics: Semantics = DefaultSemantics()
  val cCode =
    """
int foo(
#ifdef A
#ifdef B
        int x,
        #endif
        #endif
        #ifdef A
        #ifdef C
        int y,
        #endif
        #endif
        #ifdef A
        int z,
        #endif
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


/*  val stringReader = new StringReader(cCode)

  val dummyFile = new File("test.c")
  val sup = new SuperC()
  sup.init()
  sup.prepare()

  val superCParseResult = sup.parse(stringReader, dummyFile)
  val superCpg = newEmptyCpg(None)
  new MetaDataPass(superCpg, Languages.NEWC, "config.inputPath").createAndApply()

  val globalSuperC: CGlobal = new CGlobal()
  val vAstCreator = VAstCreator("test.c", globalSuperC, superCParseResult)
  val diffGraph: DiffGraphBuilder = vAstCreator.createAst()
  flatgraph.DiffGraphApplier.applyDiff(superCpg.graph, diffGraph)
  val bla = superCpg.graph._nodes(25).asInstanceOf[Iterator[nodes.Method]]
  print(DotAstGenerator.dotAst(bla).mkString)*/

  val superCDotStringVAST = generateVASTDot(cCode)
  println("\n\n\nSuperC (V)AST:")
  println(superCDotStringVAST)
  println()
  println()
  println()
  println()

  val superCDotStringVCFG = generateVCFGDot(cCode)
  println("\n\n\nSuperC (V)CFG:")
  println(superCDotStringVCFG)
}
