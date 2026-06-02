package io.joern.c2cpg.variability.vast

import io.joern.c2cpg.testfixtures.C2CpgSuite
import io.joern.c2cpg.variability.util.TestUtil.generateVASTDot
import io.joern.x2cpg.testfixtures.TestCpg
import io.shiftleft.codepropertygraph.generated.nodes.Method
import io.shiftleft.codepropertygraph.generated.nodes
import io.shiftleft.semanticcpg.dotgenerator.DotAstGenerator


class testSimpleMultiVariableDeclarationAndInitialization extends C2CpgSuite(withOssDataflow = true) {
  val cFilename: String = "test_c_file.c"
  val cCode: String =
    """
      | int a, b;
      | char c, d = 'w';
      | long l = 6, k;
      | long m = 4, n = 3;
      | char x, y, z;
      | int o = 2, p = 3, q = 4;
      | long t = a + b + 3;
      | void i() {
      |   a = 49;
      |   b = 3 + a;
      | }
      |""".stripMargin
  val cCpg: TestCpg = code(cCode, cFilename)
  val cTraversal: Iterator[Method] = cCpg.graph._nodes(25).asInstanceOf[Iterator[nodes.Method]]
  val cAstDotString: Iterator[String] = DotAstGenerator.dotAst(cTraversal, extended_view = true)
  println("Standard Joern C AST:")
  println(cAstDotString.mkString)


  val (superCAstDotString, superCJoernAstDotString) = generateVASTDot(cCode, cFilename)
  println("\n\n\nSuperC (V)AST (original data structure):")
  println(superCAstDotString)

  println("\n\n\nSuperC (V)AST (translated to JOERN VAST data structure):")
  println(superCJoernAstDotString)
}
