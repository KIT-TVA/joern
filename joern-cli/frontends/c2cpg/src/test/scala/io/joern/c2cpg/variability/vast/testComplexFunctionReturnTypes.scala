package io.joern.c2cpg.variability.vast

import io.joern.c2cpg.testfixtures.C2CpgSuite
import io.joern.c2cpg.variability.util.TestUtil.generateVASTDot
import io.joern.x2cpg.testfixtures.TestCpg
import io.shiftleft.codepropertygraph.generated.nodes
import io.shiftleft.codepropertygraph.generated.nodes.Method
import io.shiftleft.semanticcpg.dotgenerator.DotAstGenerator

class testComplexFunctionReturnTypes extends C2CpgSuite(withOssDataflow = true) {

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
      |int ***test3(int i) {
      |  i = 0;
      |  return i;
      |}
      |
      |int (test4(int i))[2][3][4] {
      |  i = 0;
      |  return i;
      |}
      |
      |int ****(test5(int i))[5][6][7] {
      |  i = 0;
      |  return i;
      |}
      |
      |unsigned int ****(test6(int ****i[2][3][4]))[8][9][10] {
      |  i = 0;
      |  return i;
      |  asm ("addl %1, %0"
      |    : "+r" (dst)  // %0: output, read/write
      |    : "r" (src)   // %1: input, read-only
      |    : "cc"        // Clobber: tells compiler the condition codes/flags changed
      |  );
      |}
      |
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
