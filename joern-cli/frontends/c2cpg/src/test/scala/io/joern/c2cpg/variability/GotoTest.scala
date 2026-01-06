package io.joern.c2cpg.variability

import io.joern.c2cpg.astcreation.{CGlobal, VAstCreator}
import io.joern.c2cpg.parser.FileDefaults
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
import io.shiftleft.semanticcpg.dotgenerator.DotCfgGenerator
import superc.SuperC

import java.io.{File, StringReader}

class GotoTest extends C2CpgSuite(withOssDataflow = true){
/*  val cCode =
    """
void foo() {

    int x = 10;
    if (x > 5){
      goto label;
    }
    else{
      goto label2;
    }
    printf("1");
    label:
    printf("2");
    label2:
    printf("3");
}"""*/

  val cCode = """
               int foo(){

                int a = 42;
                goto label;
                #ifdef MACRO1
                label:
                printf("%i", a);
                return 0;
                #endif

                #ifdef MACRO2
                label:
                printf("%i", a);
                return 3;
                #endif

                return 5;


                }"""
/*  val cCpg = code(cCode)
  val cTraversal = cCpg.graph._nodes(25).asInstanceOf[Iterator[nodes.Method]]
  val cAstDotString = DotCpg14Generator.toDotCpg14(cTraversal)
//  val cAstDotString = DotCfgGenerator.dotCfg(cTraversal)
  println(cAstDotString.mkString)*/


  implicit val semantics: Semantics = DefaultSemantics()
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
  val superCTraversal = superCpg.graph._nodes(25).asInstanceOf[Iterator[nodes.Method]]
  val superCAstDotString = DotDdgGenerator.toDotDdg(superCTraversal).mkString

  println(superCAstDotString)




}
