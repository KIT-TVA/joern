package io.joern.c2cpg.variability

import flatgraph.Graph
import io.joern.c2cpg.astcreation.{AstCreator, CGlobal, VAstCreator}
import io.joern.c2cpg.testfixtures.C2CpgSuite
import io.joern.dataflowengineoss.dotgenerator.DotCpg14Generator
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
import io.shiftleft.codepropertygraph.generated.{Cpg, DiffGraphBuilder, nodes}
import io.shiftleft.codepropertygraph.generated.nodes.*
import superc.SuperC

import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters.IteratorHasAsScala

class TrivialCpgTest extends C2CpgSuite {


  val cCode =
    """

                  int main(char a, int b) {
                     #ifdef MACRO
                          printf("a");
                     #else
                          a = a + 1;
                          printf("b");
                    #endif
                    return a;
                  }
                  """
  val stringReader = new StringReader(cCode)
  val dummyFile = new File("test.c")

  val sup = new SuperC()
  sup.init()
  sup.prepare()
  val superCParseResult = sup.parse(stringReader, dummyFile)
  var globalSuperC: CGlobal = new CGlobal()
  val vAstCreator = VAstCreator("test.c", globalSuperC, superCParseResult)
  val diffGraph = vAstCreator.createAst()
  val superCpg = newEmptyCpg(None)
  flatgraph.DiffGraphApplier.applyDiff(superCpg.graph, diffGraph)


  // Get dot representation
  val astDotGenerator = DotCpg14Generator

  val superCTraversal = superCpg.graph._nodes(25).asInstanceOf[Iterator[nodes.Method]]
  val superCAstDotString = DotCpg14Generator.toDotCpg14(superCTraversal)
  superCpg.close()

  val cCpg = code(cCode)
  val cTraversal = cCpg.graph._nodes(25).asInstanceOf[Iterator[nodes.Method]]
  val cAstDotString = DotCpg14Generator.toDotCpg14(cTraversal)
  cCpg.close()
  
  val superCSstring = superCAstDotString.mkString
  val cString = cAstDotString.mkString
  println("SuperC:")
  print(superCSstring)
  println()
  println("JoernC:")
  print(cString)

}