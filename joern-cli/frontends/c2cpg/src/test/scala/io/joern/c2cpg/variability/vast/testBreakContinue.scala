package io.joern.c2cpg.variability.vast

import io.joern.c2cpg.testfixtures.C2CpgSuite
import io.joern.c2cpg.variability.util.TestUtil.generateVASTDot

/** Task 21: break / continue inside while and for (plain + conditional). */
class testBreakContinue extends C2CpgSuite(withOssDataflow = true) {

  // No plain `if` (Task 12 unfinished) — put break/continue directly in loop bodies.
  val cCode: String =
    """
      |void helper(int x) {}
      |
      |void test(int n) {
      |  int i = 0;
      |  while (i < n) {
      |    helper(i);
      |    continue;
      |    break;
      |  }
      |
      |  for (i = 0; i < n; i++) {
      |#ifdef USE_BREAK
      |    break;
      |#else
      |    continue;
      |#endif
      |  }
      |}
      |""".stripMargin

  val (superCAstDotString, superCJoernAstDotString) = generateVASTDot(cCode, "test_break_continue.c")

  println("\nSuperC (V)AST (original data structure):")
  println(superCAstDotString)
  println("\nSuperC (V)AST (translated to JOERN VAST data structure):")
  println(superCJoernAstDotString)

  println(
    """
      |
      |=== Task 21 break/continue checklist (Joern, method `test`) ===
      |
      |1) while body:
      |   CALL helper(i)
      |   CONTROL_STRUCTURE TYPE=CONTINUE  (code "continue;")
      |   CONTROL_STRUCTURE TYPE=BREAK     (code "break;")
      |
      |2) for body with #ifdef USE_BREAK:
      |   CHOICE under FOR body
      |   branch USE_BREAK: CONTROL_STRUCTURE BREAK
      |   else branch: CONTROL_STRUCTURE CONTINUE
      |
      |3) SuperC original:
      |   Search BreakStatement / ContinueStatement
      |
      |""".stripMargin
  )
}

