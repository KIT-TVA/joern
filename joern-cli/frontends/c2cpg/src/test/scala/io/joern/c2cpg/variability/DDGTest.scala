package io.joern.c2cpg.variability

import io.joern.c2cpg.astcreation.{CGlobal, VAstCreator}
import io.joern.c2cpg.parser.FileDefaults
import io.joern.c2cpg.testfixtures.{AstC2CpgSuite, C2CpgSuite, CDefaultTestCpg}
import io.joern.dataflowengineoss.DefaultSemantics
import io.joern.dataflowengineoss.dotgenerator.DotDdgGenerator
import io.joern.dataflowengineoss.passes.reachingdef.ReachingDefPass
import io.joern.dataflowengineoss.semanticsloader.Semantics
import io.joern.x2cpg.X2Cpg
import io.joern.x2cpg.X2Cpg.newEmptyCpg
import io.joern.x2cpg.passes.frontend.MetaDataPass
import io.joern.x2cpg.testfixtures.Code2CpgFixture
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.{DiffGraphBuilder, Languages, nodes}
import io.shiftleft.semanticcpg.dotgenerator.DotAstGenerator
import superc.SuperC

import java.io.{File, StringReader}

class DDGTest extends C2CpgSuite(withOssDataflow = true){
  val cCode =
    """
void foo() {
    int x = 5;
       printf("%i", x);

}"""

  val cCpg = code(cCode)
  val cTraversal = cCpg.graph._nodes(25).asInstanceOf[Iterator[nodes.Method]]
  val cAstDotString = DotAstGenerator.dotAst(cTraversal)
  println(cAstDotString.mkString)
  println()
  println()
  println()
  println()
  println()

  val stringReader = new StringReader(cCode)
  val dummyFile = new File("test.c")

/*  val sup = new SuperC()
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
    implicit val semantics: Semantics = DefaultSemantics()
    new ReachingDefPass(superCpg).createAndApply()
  val superCTraversal = superCpg.graph._nodes(25).asInstanceOf[Iterator[nodes.Method]]
//  val superCAstDotString = DotDdgGenerator.toDotDdg(superCTraversal).mkString
val superCAstDotString = DotAstGenerator.dotAst(superCTraversal).mkString
  println(superCAstDotString)*/
}
