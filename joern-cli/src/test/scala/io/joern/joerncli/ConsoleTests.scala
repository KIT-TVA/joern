package io.joern.joerncli

import flatgraph.Graph
import io.joern.dataflowengineoss.dotgenerator.DotCpg14Generator
import io.shiftleft.semanticcpg.utils.FileUtil.*
import io.shiftleft.semanticcpg.utils.FileUtil
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
//import xtc.tree.*
//import xtc.util.Pair
import io.shiftleft.semanticcpg.dotgenerator.DotSerializer

import java.io.{File, StringReader}
import java.nio.file.{Files, Path}
import io.joern.x2cpg.*
import io.joern.x2cpg.X2Cpg.newEmptyCpg
import io.shiftleft.codepropertygraph.generated.{Cpg, DiffGraphBuilder}
import io.shiftleft.codepropertygraph.generated.nodes.*
//import superc.SuperC
class ConsoleTests extends AnyWordSpec with Matchers {

  if (scala.util.Properties.isWin) {
    info(
      "console tests don't work on windows - not sure why... running the console manually works though: try the `run` and `help.cpg` commands in joern"
    )
  } else {
    "run" should {
      "provide a human readable overview of overlay creators" in withTestCode { codeDir =>
        RunScriptTests.exec("general/run.sc", codeDir.toString)
      }
    }

    "help" should {
      "allow getting long description via help object" in withTestCode { codeDir =>
        RunScriptTests.exec("general/help.sc", codeDir.toString)
      }
    }
  }

  def withTestCode(fun: Path => Unit): Unit = {
    FileUtil.usingTemporaryDirectory("console") { workspaceDir =>
      FileUtil.usingTemporaryDirectory("console") { codeDir =>
        Files.createDirectory(codeDir / "dir1")
        Files.createDirectory(codeDir / "dir2")

        val fooDir     = (codeDir / "dir1" / "foo.c")
        val fooContent = "int main(int argc, char **argv) { char *ptr = 0x1 + argv; return argc; }"

        Files.writeString(fooDir, fooContent)

        val barDir     = (codeDir / "dir2" / "bar.c")
        val barContent = "int bar(int x) { return x; }"

        Files.writeString(barDir, barContent)

        fun(codeDir)
      }
    }
   /* val sup = new SuperC()
    sup.init()
    sup.prepare()
    val cCode = """
            int main() {
                int x = 20;
                int y = x * 2;
                if (x<y){
                  y = x;
                }
                printf("Hello World");
                return 0;
            }
            """

    val stringReader = new StringReader(cCode)
    val dummyFile = new File("test.c")

    val result = sup.parse(stringReader, dummyFile)

    //var nodes : Seq[Node] = Seq(result)
    //while (nodes.nonEmpty){
    //  //TODO: do something with the nodes
    //  nodes = nodes.flatMap(node => (0 until node.size).map(i => node.getNode(i)))
    //}


    //result match {
    //  case node: xtc.tree.GNode => println(s"Found GNode: ${node.getName}")
    //  case node: xtc.tree.Annotation => println(s"Found Annotation: ${node.getName}")
    //  case node: xtc.tree.Attribute => println(s"Found Attribute: ${node.getName}")
    //  case node: xtc.tree.Comment => println(s"Found Comment: ${node.getText}")
    //  case node: xtc.tree.Locator => println(s"Found Locator at line: ${node.line}")
    //  case node: xtc.tree.Property => println(s"Found Property: ${node.getName}")
    //  case node: xtc.tree.Token => println(s"Found Token: ${node.getText}")
    //  case node: Node => println(s"Found generic Node: ${node.toString}")
    //  case _ => println("Unknown node type")
    //}

    //def convertToJoernAST(root : Node): Ast = {
//
    //}

    def convertXTCNodeToJoern(node: Node): NewNode = {
      node match
      {
        case node: GNode if node.hasName("FunctionDefinition") =>
          val functionPrototype: Node = node.getNode(0)
          val returnType = functionPrototype.getNode(0).getString(0)
          val name = functionPrototype.getNode(1).getNode(0).getString(0)
          NewCall()
            .name(name)
            .typeFullName(returnType)
        case _ => NewReturn()
      }
    }
    val diffGraph: DiffGraphBuilder = Cpg.newDiffGraphBuilder
    val test = convertXTCNodeToJoern(result.getNode(0).getNode(0).getNode(1))
    implicit val validationMode: ValidationMode = ValidationMode.Disabled
    val bla = Ast(test)

    val fileNode = NewFile().name("test").order(0)
    val testAst = Ast(fileNode).withChild(bla)
    Ast.storeInDiffGraph(testAst, diffGraph)

    val cpg = newEmptyCpg(None)
    flatgraph.DiffGraphApplier.applyDiff(cpg.graph, diffGraph)
    cpg.graph
    bla.
    //val astDotGenerator = DotCpg14Generator()
    //val astDotString = astDotGenerator.generate(cpg.)



    //cpg.method.ast.isControlStructure.code(".*y > 42.*").dotAst.l
    cpg.close()

    //diffGraph.build(graph)
    //val cpg = new Cpg(graph)




    result.getNode(0).getNode(0).getNode(1) match {
      case node: GNode if node.hasName("FunctionDefinition") => print(node);
      case _ => print("not FunctionDef: ")
        print

    }

    System.out.println("Parse result: " + result)*/
  }
}
