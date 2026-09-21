package io.joern.c2cpg.variability.vast.demo

import io.joern.c2cpg.testfixtures.C2CpgSuite
import io.joern.c2cpg.variability.util.TestUtil.generateVASTDot
import io.joern.dataflowengineoss.dotgenerator.DotCpg14Generator
import io.joern.x2cpg.testfixtures.TestCpg
import io.shiftleft.codepropertygraph.generated.nodes
import io.shiftleft.codepropertygraph.generated.nodes.Method
import io.shiftleft.semanticcpg.dotgenerator.DotAstGenerator

class VASTConditionalSample extends C2CpgSuite(withOssDataflow = true) {
  val cFilename: String = "conditional_sample.c"
  val cCode: String =
    """
      | void foo() {
      |   int x = source(); // Attacker−controlled.
      |   if(x < MAX){ // Does not enforce x >= 0.
      |     int y = 0;
      | #ifdef CONFIG_PROCESS_INPUT
      |     y = 2 * x;
      |   #ifdef CONFIG_SEND_DATA
      |     sink(y); // Security−sensitive operation.
      |   #endif
      | #endif
      |     // ...
      |   }
      | }
      |
      |""".stripMargin
  val cCpg: TestCpg = code(cCode, cFilename)
  val cTraversal: Iterator[Method] = cCpg.graph._nodes(25).asInstanceOf[Iterator[nodes.Method]]
  val cAstDotString: Iterator[String] = DotAstGenerator.dotAst(cTraversal, extendedView = true, withColoring = true)
  println("Standard Joern C AST:")
  println(cAstDotString.mkString)


  val (superCAstDotString, superCJoernAstDotString) = generateVASTDot(cCode, cFilename)
  println("\n\n\nSuperC (V)AST (original data structure):")
  println(superCAstDotString)

  println("\n\n\nSuperC (V)AST (translated to JOERN VAST data structure):")
  println(superCJoernAstDotString)
}
