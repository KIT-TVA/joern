package io.joern.c2cpg.variability.vast

import io.joern.c2cpg.testfixtures.C2CpgSuite
import io.joern.c2cpg.variability.util.TestUtil.generateVASTDot

/** Task 12: if / else if / else (plain + conditional condition). */
class testIf extends C2CpgSuite(withOssDataflow = true) {

  val cCode: String =
    """
      |void helper(int x) {}
      |
      |void test(int x, int y) {
      |  if (x > 0) {
      |    helper(x);
      |  }
      |
      |  if (x > y) {
      |    helper(x);
      |  } else if (x < y) {
      |    helper(y);
      |  } else {
      |    helper(0);
      |  }
      |
      |  if (
      |#ifdef USE_X
      |x
      |#else
      |y
      |#endif
      |  > 0) {
      |    helper(x);
      |  } else {
      |    helper(y);
      |  }
      |}
      |""".stripMargin

  val (superCAstDotString, superCJoernAstDotString) = generateVASTDot(cCode, "test_if.c")

  println("\nSuperC (V)AST (original data structure):")
  println(superCAstDotString)
  println("\nSuperC (V)AST (translated to JOERN VAST data structure):")
  println(superCJoernAstDotString)

  println(
    """
      |
      |=== Task 12 if/else checklist (Joern dot, method `test`) ===
      |
      |1) Plain if (x > 0):
      |   CONTROL_STRUCTURE TYPE=IF, condition greaterThan(x, 0)
      |   then body BLOCK with CALL helper(x)
      |
      |2) if / else if / else chain:
      |   IF (x > y) -> then helper(x)
      |   ELSE child -> nested IF (x < y) -> then helper(y)
      |   ELSE child -> BLOCK with CALL helper(0)
      |
      |3) Conditional in if condition (USE_X):
      |   IF under which condition has CHOICE (IDENTIFIER x vs y), then > 0
      |   else branch CALL helper(y)
      |
      |""".stripMargin
  )
}
