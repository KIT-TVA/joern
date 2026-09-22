package io.joern.c2cpg.variability.vast

import io.joern.c2cpg.testfixtures.C2CpgSuite
import io.joern.c2cpg.variability.util.TestUtil.generateVASTDot

/** Task 8: goto + plain labels (plain). */
class testGoto extends C2CpgSuite(withOssDataflow = true) {

  val cCode: String =
    """
      |void helper(int x) {}
      |
      |void test(int x) {
      |  if (x > 0) {
      |    goto done;
      |  }
      |  helper(x);
      |done:
      |  helper(0);
      |}
      |""".stripMargin

  val (superCAstDotString, superCJoernAstDotString) = generateVASTDot(cCode, "test_goto.c")

  println("\nSuperC (V)AST (original data structure):")
  println(superCAstDotString)
  println("\nSuperC (V)AST (translated to JOERN VAST data structure):")
  println(superCJoernAstDotString)

  println(
    """
      |
      |=== Task 8 goto checklist (Joern dot, method `test`) ===
      |
      |1) CONTROL_STRUCTURE TYPE=GOTO, CODE like "goto done;"
      |2) JUMP_TARGET name=done (or CODE "done:")
      |3) CALL helper under IF then-branch path / after label
      |
      |""".stripMargin
  )
}
