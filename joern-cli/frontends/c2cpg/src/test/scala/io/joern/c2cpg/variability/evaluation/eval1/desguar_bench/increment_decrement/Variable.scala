package io.joern.c2cpg.variability.evaluation.eval1.desguar_bench.increment_decrement

import io.joern.c2cpg.astcreation.{CGlobal, VAstCreator}
import io.joern.c2cpg.testfixtures.C2CpgSuite
import io.joern.c2cpg.variability.util.TestUtil.{generateVASTDot, generateVCFGDot, generateVCPGDot, generateVDDGDot, generateVPDGDot}
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
int main() {
  int x;
  if (x) {
#ifdef A
    x++;
#endif
  } else {
#ifdef B
    x--;
#endif
  }
  return 0;
}

    """
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

  println()
  println()
  println()
  println()

  val superCDotStringVPDG = generateVPDGDot(cCode)
  println("\n\n\nSuperC (V)PDG:")
  println(superCDotStringVPDG)
}
