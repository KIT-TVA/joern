package io.joern.c2cpg.variability.vast

import io.joern.c2cpg.testfixtures.C2CpgSuite
import io.joern.c2cpg.variability.util.TestUtil.generateVASTDot
import io.joern.x2cpg.testfixtures.TestCpg
import io.shiftleft.codepropertygraph.generated.nodes.Method
import io.shiftleft.codepropertygraph.generated.nodes
import io.shiftleft.semanticcpg.dotgenerator.DotAstGenerator

class testSimpleMethodDeclaration extends C2CpgSuite(withOssDataflow = true) {
  val cFilename: String = "test_c_file.c"
  val cCode: String =
    """
      | void a() {
      |   a = 49;
      |   if (a == 2) {
      |     print("a == 2");
      |   } else {
      |     print("a != 2");
      |   }
      |   b = 3 + a;
      | }
      |
      | int b(int a1) {
      |   a = 49;
      |   return a;
      | }
      |
      | float c(int a, float b) {
      |   float c = (float) a;
      |   return c;
      | }
      |""".stripMargin
  val cCpg: TestCpg = code(cCode, cFilename)
  val cTraversal: Iterator[Method] = cCpg.graph._nodes(25).asInstanceOf[Iterator[nodes.Method]]
  val cAstDotString: Iterator[String] = DotAstGenerator.dotAst(cTraversal, extendedView = true)
  println("Standard Joern C AST:")
  println(cAstDotString.mkString)


  val (superCAstDotString, superCJoernAstDotString) = generateVASTDot(cCode, cFilename)
  println("\n\n\nSuperC (V)AST (original data structure):")
  println(superCAstDotString)

  println("\n\n\nSuperC (V)AST (translated to JOERN VAST data structure):")
  println(superCJoernAstDotString)
}
