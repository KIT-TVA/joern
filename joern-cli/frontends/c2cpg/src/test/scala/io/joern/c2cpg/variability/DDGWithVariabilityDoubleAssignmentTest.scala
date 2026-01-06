package io.joern.c2cpg.variability

import io.joern.c2cpg.astcreation.{CGlobal, VAstCreator}
import io.joern.c2cpg.parser.FileDefaults
import io.joern.c2cpg.passes.variability.PdgPresenceConditionAnnotationPass
import io.joern.c2cpg.testfixtures.{AstC2CpgSuite, C2CpgSuite, CDefaultTestCpg}
import io.joern.dataflowengineoss.DefaultSemantics
import io.joern.dataflowengineoss.dotgenerator.{DotCpg14Generator, DotDdgGenerator}
import io.joern.dataflowengineoss.passes.reachingdef.ReachingDefPass
import io.joern.dataflowengineoss.semanticsloader.Semantics
import io.joern.x2cpg.X2Cpg
import io.joern.x2cpg.X2Cpg.newEmptyCpg
import io.joern.x2cpg.passes.frontend.MetaDataPass
import io.joern.x2cpg.testfixtures.Code2CpgFixture
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.{DiffGraphBuilder, Languages, nodes}
import io.shiftleft.semanticcpg.dotgenerator.{DotAstGenerator, DotCfgGenerator}
import superc.SuperC

import java.io.{File, StringReader}

class DDGWithVariabilityDoubleAssignmentTest extends C2CpgSuite(withOssDataflow = true){
  implicit val semantics: Semantics = DefaultSemantics()
  val cCode =
    """
void foo() {
    #ifdef MACRO
       int Y = 10;
    #else
       int Y = 37;
    #endif
    bar(Y);
}"""
  val stringReader = new StringReader(cCode)

  val dummyFile = new File("test.c")
  val sup = new SuperC()
  sup.init()
  sup.prepare()

  val superCParseResult = sup.parse(stringReader, dummyFile)
  var superCpg = newEmptyCpg(None)
  new MetaDataPass(superCpg, Languages.NEWC, "config.inputPath").createAndApply()

  var globalSuperC: CGlobal = new CGlobal()
  val vAstCreator = VAstCreator("test.c", globalSuperC, superCParseResult)
  val diffGraph: DiffGraphBuilder = vAstCreator.createAst()
  flatgraph.DiffGraphApplier.applyDiff(superCpg.graph, diffGraph)
  X2Cpg.applyDefaultOverlays(superCpg)

  new ReachingDefPass(superCpg).createAndApply()
  //new PdgPresenceConditionAnnotationPass(superCpg).createAndApply()
  val superCTraversal = superCpg.graph._nodes(25).asInstanceOf[Iterator[nodes.Method]]
//  val superCAstDotString = DotCpg14Generator.toDotCpg14(superCTraversal).mkString
  val superCAstDotString = DotAstGenerator.dotAst(superCTraversal).mkString
//  val superCAstDotString = DotCfgGenerator.dotCfg(superCTraversal).mkString
//  val superCAstDotString = DotDdgGenerator.toDotDdg(superCTraversal).mkString


  println(superCAstDotString)
}
//  val cCpg = code(cCode)
//  val cTraversal = cCpg.graph._nodes(25).asInstanceOf[Iterator[nodes.Method]]
//  val cAstDotString = DotDdgGenerator.toDotDdg(cTraversal)
//  println(cAstDotString.mkString)
