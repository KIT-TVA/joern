package io.joern.c2cpg.variability.vast

import io.joern.c2cpg.testfixtures.C2CpgSuite
import io.joern.c2cpg.variability.util.TestUtil.generateVASTDot
import io.joern.x2cpg.testfixtures.TestCpg
import io.shiftleft.codepropertygraph.generated.nodes
import io.shiftleft.codepropertygraph.generated.nodes.Method
import io.shiftleft.semanticcpg.dotgenerator.DotAstGenerator

class testComplexConditionalFunctionReturnTypes extends C2CpgSuite(withOssDataflow = true) {

  val cCode: String =
    """
      |
      |int test1(int i) {
      |  i = 0;
      |  return i;
      |}
      |
      |unsigned int test2(int i) {
      |  i = 0;
      |  return i;
      |}
      |
      |enum color test3(int i) {
      |  i = 0;
      |  return i;
      |}
      |
      |struct Point test4(int i) {
      |  i = 0;
      |  return i;
      |}
      |
      |#if M0
      |struct Point
      |#elif M1
      |enum color
      |#elif M2
      |int
      |#else
      |unsigned int
      |#endif
      |test5(int i) {
      |  i = 0;
      |  return i;
      |}
      |
      |int ****(test6(int i))
      |#if m3
      |[2][3]
      |#endif
      |[4]
      |{
      |  i = 0;
      |  return i;
      |}
      |
      |unsigned int ****(
      |#if m0
      |test6
      |#else
      |test66
      |#endif
      |(int i))[8][9][10] {
      |  i = 0;
      |  return i;
      |
      |}
      |
      |// The following test is at the moment not supported.
      |/**
      |int
      |#if m4
      |**
      |#elif m5
      |**++
      |#endif
      |test7(int i) {
      |  i = 0;
      |  return i;
      |}
      |**/
      |
      |""".stripMargin

  val cCpg: TestCpg = code(cCode, "Test.c")
  val cTraversal: Iterator[Method] = cCpg.graph._nodes(25).asInstanceOf[Iterator[nodes.Method]]
  val cAstDotString: Iterator[String] = DotAstGenerator.dotAst(cTraversal, extendedView = true, withColoring = true)
  println("Standard Joern C AST:")
  println(cAstDotString.mkString)

  val (superCAstDotString, superCJoernAstDotString) = generateVASTDot(cCode, "test_enum.c")

  println("\nSuperC (V)AST (original data structure):")
  println(superCAstDotString)
  println("\nSuperC (V)AST (translated to JOERN VAST data structure):")
  println(superCJoernAstDotString)

}
