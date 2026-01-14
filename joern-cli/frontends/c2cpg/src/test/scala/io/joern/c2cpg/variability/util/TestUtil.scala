package io.joern.c2cpg.variability.util

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
import io.shiftleft.codepropertygraph.generated.nodes.{Method, StoredNode}
import io.shiftleft.codepropertygraph.generated.{DiffGraphBuilder, Languages, nodes}
import io.shiftleft.semanticcpg.dotgenerator.{DotAstGenerator, DotCfgGenerator}
import superc.SuperC

import java.io.{File, StringReader}


object TestUtil {

  def generateVASTDot(cCode: String): String = {
    val vcpg = generateVCPG(cCode)
    DotAstGenerator.dotAst(vcpg).mkString
  }

  def generateVCFGDot(cCode: String): String = {
    val vcpg = generateVCPG(cCode)
    DotCfgGenerator.dotCfg(vcpg).mkString
  }

  def generateVDDGDot(cCode: String): String = {
    val vcpg = generateVCPG(cCode)
    DotDdgGenerator.toDotDdg(vcpg).mkString
  }


  def generateVCPGDot(cCode: String): String = {
    val vcpg = generateVCPG(cCode)
    DotCpg14Generator.toDotCpg14(vcpg).mkString
  }

  def generateVCPG(cCode: String):  Iterator[Method] = {
    implicit val semantics: Semantics = DefaultSemantics()
    val stringReader = new StringReader(cCode)

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
    X2Cpg.applyDefaultOverlays(superCpg)

    new ReachingDefPass(superCpg).createAndApply()
    new PdgPresenceConditionAnnotationPass(superCpg).createAndApply()
    superCpg.graph._nodes(25).asInstanceOf[Iterator[nodes.Method]]
  }
}
