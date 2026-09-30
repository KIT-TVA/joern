package io.joern.c2cpg.variability.vast

import io.joern.c2cpg.testfixtures.C2CpgSuite
import io.joern.c2cpg.variability.util.TestUtil.generateVASTDot

/** Task 13 follow-up: #ifdef inside switch case / body → CHOICE (Blöcke Variabilität). */
class testConditionalSwitch extends C2CpgSuite(withOssDataflow = true) {

  val cCode: String =
    """
      |void helper(int x) {}
      |
      |void test(int x) {
      |  switch (x) {
      |    case 0:
      |#ifdef FEATURE_A
      |      helper(0);
      |#else
      |      helper(10);
      |#endif
      |      break;
      |    case 1:
      |      helper(1);
      |      break;
      |#ifdef FEATURE_B
      |    default:
      |      helper(x);
      |      break;
      |#endif
      |  }
      |}
      |""".stripMargin

  val (superCAstDotString, superCJoernAstDotString) = generateVASTDot(cCode, "test_conditional_switch.c")

  println("\nSuperC (V)AST (original data structure):")
  println(superCAstDotString)
  println("\nSuperC (V)AST (translated to JOERN VAST data structure):")
  println(superCJoernAstDotString)

  println(
    """
      |
      |=== Task 13 conditional-switch checklist (Joern, method `test`) ===
      |
      |1) CONTROL_STRUCTURE TYPE=SWITCH still present
      |2) case 0 body: CHOICE — helper(0) vs helper(10) under FEATURE_A
      |3) case 1: JUMP_TARGET + helper(1) + BREAK
      |4) default under FEATURE_B: CHOICE wrapping JUMP_TARGET default / helper(x)
      |5) LINE_NUMBER on SWITCH / JUMP_TARGET should not all be placeholder 42
      |
      |""".stripMargin
  )
}
