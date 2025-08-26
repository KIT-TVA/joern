package io.joern.c2cpg.variability

import flatgraph.Graph
import io.joern.c2cpg.astcreation.VAstCreator
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

object TrivialCpgTest {
    /*
    def createTrivialCpg(): Cpg = {
        val cpg = Cpg.empty
        val diffGraph = Cpg.newDiffGraphBuilder

        // Add a file node (required for a valid CPG)
        val fileNode = NewFile()
          .name("trivial.scala")
          .content("1 == 1")
        diffGraph.addNode(fileNode)

        // Add a method to contain our expression
        val methodNode = NewMethod()
          .name("printMethod")
          .fullName("printMethod")
          .code("println(\"Hello World\")")
        diffGraph.addNode(methodNode)

        // Add the println call
        val printCallNode = NewCall()
          .name("println")
          .methodFullName("scala.Predef.println")
          .code("println(\"Hello World\")")
          .typeFullName("Unit")
        diffGraph.addNode(printCallNode)

        // Add string literal for the argument
        val stringLiteral = NewLiteral()
          .code("\"Hello World\"")
          .typeFullName("String")
        diffGraph.addNode(stringLiteral)

        // Add method parameter if needed
        val parameterNode = NewMethodParameterIn()
          .name("x")
          .typeFullName("Any")
          .order(1)
        diffGraph.addNode(parameterNode)

        // Add method return node
        val methodReturnNode = NewMethodReturn()
          .typeFullName("Unit")
        diffGraph.addNode(methodReturnNode)

        // Create AST edges to connect the nodes
        diffGraph.addEdge(methodNode, printCallNode, EdgeTypes.AST)
        diffGraph.addEdge(printCallNode, stringLiteral, EdgeTypes.AST)
        diffGraph.addEdge(methodNode, parameterNode, EdgeTypes.AST)
        diffGraph.addEdge(methodNode, methodReturnNode, EdgeTypes.AST)

        // Add argument edge for the string literal
        diffGraph.addEdge(printCallNode, stringLiteral, EdgeTypes.ARGUMENT)

        // Apply the changes to the CPG
        DiffGraphApplier.applyDiff(cpg.graph, diffGraph)

        cpg
    }
*/
    def main(args: Array[String]): Unit = {
        val sup = new SuperC()
        sup.init()
        sup.prepare()
        val cCode =
            """
            #ifdef TEST
            struct A {
            int x;
            };
            #else
            struct A {
            float y;
            };
            #endif

                  int main() {
                      struct A a;
                  }
                  """

        val stringReader = new StringReader(cCode)
        val dummyFile = new File("test.c")

        val result = sup.parse(stringReader, dummyFile)

        val vAstCreator = VAstCreator("test.c", result)
        val diffGraph = vAstCreator.createAst()

        val cpg = newEmptyCpg(None)
        val traversal = cpg.graph._nodes(25).asInstanceOf[Iterator[nodes.Method]]

        flatgraph.DiffGraphApplier.applyDiff(cpg.graph, diffGraph)
        cpg.graph

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
        System.out.println("Parse result: " + result)

        /*val cpg = createTrivialCpg()
        try {
            val nodeStarters = new CpgNodeStarters(cpg)
            println(s"Total nodes: ${nodeStarters.all.map(_.toString).toString()}")
            println(s"Calls: ${nodeStarters.call.code.toList}")
            println(s"Literals: ${nodeStarters.literal.code.toList}")
            ///print(cpg.graph.allNodes().map(_.toString))
            cpg.graph.allNodes.foreach { node =>
                println(s"${node.label}: ${node}")
            }

        } finally {
            cpg.close()
        }*/
    }
}