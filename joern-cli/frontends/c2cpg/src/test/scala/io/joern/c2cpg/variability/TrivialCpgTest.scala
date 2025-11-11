package io.joern.c2cpg.variability

import flatgraph.Graph
import io.joern.c2cpg.astcreation.{AstCreator, CGlobal, VAstCreator}
import io.joern.c2cpg.passes.PresenceConditionPass
import io.joern.c2cpg.testfixtures.C2CpgSuite
import io.joern.dataflowengineoss.dotgenerator.{DotCpg14Generator, DotPdgGenerator}
import io.shiftleft.semanticcpg.utils.FileUtil.*
import io.shiftleft.semanticcpg.utils.FileUtil
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import xtc.tree.*
import xtc.util.Pair
import io.shiftleft.semanticcpg.dotgenerator.DotSerializer

import java.io.{File, StringReader}
import java.nio.file.{Files, Path}
import io.joern.x2cpg.*
import io.joern.x2cpg.X2Cpg.newEmptyCpg
import io.joern.x2cpg.passes.frontend.MetaDataPass
import io.shiftleft.codepropertygraph.generated.{Cpg, DiffGraphBuilder, Languages, nodes}
import io.shiftleft.codepropertygraph.generated.nodes.*
import superc.SuperC

import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters.IteratorHasAsScala

class TrivialCpgTest extends C2CpgSuite {

/*
* int main(char a, int b) {
                     #ifdef MACRO
                          printf("a");
                     #else
                          a = a + 1;
                          printf("b");
                    #endif
                    return a;
                  }
* */

  /*    """

                  int main(char a, int b) {
    if (a > 9){
      printf("9");
    }
    #ifdef MACRO
      if
    #else
      else if
    #endif
    (a == b){
     printf("a");
    }
    else{
     printf("b");
    }

                    return a;
                  }
                  """*/


  val cCode =
    """

int main(char a, int b) {
  #ifdef MACRO
       printf("a");
  #else
       printf("b");
  #endif
  return a;
}
"""

/*
  val cCode =
    """

int main(char a, int b) {
    if(b >5){
    goto start;
    }
    printf("test");
#ifdef MACRO
    start;
     printf("a");
#else
    start;
     printf("b");
#endif
return a;
}
"""
*/


/*  val cCode =
    """

int main(char a, int b) {
  int t = 21;
    if (a > b){
    t = b;
    }
    printf("%i", t);
  return a;
}
"""*/
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
  val diffGraph = vAstCreator.createAst()
  flatgraph.DiffGraphApplier.applyDiff(superCpg.graph, diffGraph)
  new PresenceConditionPass(superCpg).createAndApply()
  X2Cpg.applyDefaultOverlays(superCpg)

  // Get dot representation

  val superCTraversal = superCpg.graph._nodes(25).asInstanceOf[Iterator[nodes.Method]]
  val superCAstDotString = DotCpg14Generator.toDotCpg14(superCTraversal)
//  val superCAstDotString = DotPdgGenerator.toDotPdg(superCTraversal)
  superCpg.close()

  val cCpg = code(cCode)
  val cTraversal = cCpg.graph._nodes(25).asInstanceOf[Iterator[nodes.Method]]
//  val cAstDotString = DotPdgGenerator.toDotPdg(cTraversal)
  val cAstDotString = DotCpg14Generator.toDotCpg14(cTraversal)
  cCpg.close()
  
  val superCSstring = superCAstDotString.mkString
  val cString = cAstDotString.mkString
//  println("SuperC:")
  print(superCSstring)
  println()
  println()
  println()
  println()
  println()
  println("JoernC:")
  print(cString)

}