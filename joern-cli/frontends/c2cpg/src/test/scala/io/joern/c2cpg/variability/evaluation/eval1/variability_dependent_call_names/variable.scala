package io.joern.c2cpg.variability.evaluation.eval1.variability_dependent_call_names

import io.joern.c2cpg.astcreation.{CGlobal, VAstCreator}
import io.joern.c2cpg.testfixtures.C2CpgSuite
import io.joern.c2cpg.variability.util.TestUtil.{generateVASTDot, generateVCFGDot, generateVCPGDot, generateVPDGDot}
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

class variable extends C2CpgSuite(withOssDataflow = true) {
  implicit val semantics: Semantics = DefaultSemantics()
  val cCode =
    """
    void main(int a){
    #ifdef macro
    foo
    #else
    bar
    #endif
    (a);
    printf("42");
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
