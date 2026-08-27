package io.joern.c2cpg.variability.vast

import io.joern.c2cpg.testfixtures.C2CpgSuite
import io.joern.c2cpg.variability.util.TestUtil.generateVASTDot
import io.joern.x2cpg.testfixtures.TestCpg
import io.shiftleft.codepropertygraph.generated.nodes
import io.shiftleft.codepropertygraph.generated.nodes.Method
import io.shiftleft.semanticcpg.dotgenerator.DotAstGenerator

class testWhile extends C2CpgSuite(withOssDataflow = true) {

  // Task 19: while / do-while + conditional macros in condition/body (same idea as Task 16 nested args).
  val cCode: String =
    """
      |void helper(int x) {}
      |
      |void test(int x, int y) {
      |  while (x > 0) {
      |    helper(x);
      |    x--;
      |  }
      |  do {
      |    helper(x);
      |    x--;
      |  } while (x > 0);
      |
      |  while (
      |#ifdef USE_XSSSSS
      |x < 1
      |#else
      |y > 0
      |#endif
      |  ) {
      |    #if USE_A
      |    helper(x);
      |#if USE_B
      |    x--;
      |#else
      |    x++;
      |#endif
      |#endif
      |  }
      |
      |  do {
      |#if USE_A
      |    helper(x);
      |#if USE_B
      |    x--;
      |#else
      |    x++;
      |#endif
      |#endif
      |  } while (
      |  #ifdef USE_XSSSSS
      |x < 1
      |#else
      |y > 0
      |#endif
      |  );
      |}
      |""".stripMargin

  val cCpg: TestCpg = code(cCode, "test_while.c")
  val cTraversal: Iterator[Method] = cCpg.graph._nodes(25).asInstanceOf[Iterator[nodes.Method]]
  val cAstDotString: Iterator[String] = DotAstGenerator.dotAst(cTraversal, extended_view = true)
  println("Standard Joern C AST:")
  println(cAstDotString.mkString)

  val (superCAstDotString, superCJoernAstDotString) = generateVASTDot(cCode, "test_while.c")

  println("\nSuperC (V)AST (original data structure):")
  println(superCAstDotString)
  println("\nSuperC (V)AST (translated to JOERN VAST data structure):")
  println(superCJoernAstDotString)

  println(
    """
      |
      |=== Task 19 while/do-while checklist (Joern dot, method `test`) ===
      |
      |1) Plain while:
      |   CONTROL_STRUCTURE TYPE=WHILE, CODE contains "while" and "x > 0"
      |   condition: greaterThan; body BLOCK with helper(x) and x--
      |
      |2) Plain do-while:
      |   CONTROL_STRUCTURE TYPE=DO
      |   body BLOCK with helper(x) and x--; condition x > 0 (often last child)
      |
      |3) Conditional in while condition (USE_X):
      |   WHILE under which condition side has CHOICE
      |   branches: IDENTIFIER x vs IDENTIFIER y, then compared with > 0
      |
      |4) Nested conditional in do-while body (USE_A / USE_B):
      |   DO body has CHOICE under USE_A
      |   inner CHOICE: x-- vs x++ (whiteboard-style nesting like Task 16 helper2)
      |
      |5) SuperC original:
      |   Search IterationStatement; also Conditional nodes under condition/body
      |
      |""".stripMargin
  )
}
