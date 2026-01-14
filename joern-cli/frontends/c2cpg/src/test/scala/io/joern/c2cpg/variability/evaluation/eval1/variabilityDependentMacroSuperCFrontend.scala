package io.joern.c2cpg.variability.evaluation.eval1

import io.joern.c2cpg.astcreation.{CGlobal, VAstCreator}
import io.joern.c2cpg.testfixtures.C2CpgSuite
import io.joern.c2cpg.variability.util.TestUtil.{generateVASTDot, generateVCPGDot}
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

class variabilityDependentMacroSuperCFrontend extends C2CpgSuite(withOssDataflow = true) {
  implicit val semantics: Semantics = DefaultSemantics()
  val cCode =
    """
    #ifdef MACRO
      #define LIMIT 10
    #else
      #define LIMIT 2
    #endif
    void foo(int a) {
      if (a < LIMIT){
        printf("Hello World!\n");
      }
    }
    """
  val superCDotString = generateVCPGDot(cCode)

  println("\n\n\nSuperC (V)CPG:")
  println(superCDotString)


}
