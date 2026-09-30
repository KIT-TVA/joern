package io.joern.c2cpg.variability.vast

import io.joern.c2cpg.testfixtures.C2CpgSuite
import io.joern.c2cpg.variability.util.TestUtil.generateVASTDot

/**
 * Switch + Conditionals: condition, case label, optional case.
 * Case-label #ifdef should CHOICE JUMP_TARGET case 0 / case 1, not the helper CALL.
 */
class testConditionalSwitchFeatures extends C2CpgSuite(withOssDataflow = true) {

  val cCode: String =
    """
      |void helper(int x) {}
      |
      |void test(int x, int y) {
      |  /* 1) switch condition variable */
      |  switch (
      |#ifdef USE_X
      |    x
      |#else
      |    y
      |#endif
      |  ) {
      |    case 0:
      |      helper(0);
      |      break;
      |    default:
      |      helper(1);
      |      break;
      |  }
      |
      |  /* 2) case label variable — label only under #ifdef; body shared */
      |  switch (x) {
      |#ifdef CASE_ZERO
      |    case 0:
      |#else
      |    case 1:
      |#endif
      |      helper(x);
      |      break;
      |    default:
      |      helper(0);
      |      break;
      |  }
      |
      |  /* 3) which cases exist */
      |  switch (x) {
      |    case 0:
      |      helper(0);
      |      break;
      |#ifdef HAS_ONE
      |    case 1:
      |      helper(1);
      |      break;
      |#endif
      |    default:
      |      helper(x);
      |      break;
      |  }
      |}
      |""".replace("\r", "").stripMargin

  val (superCAstDotString, superCJoernAstDotString) =
    generateVASTDot(cCode, "test_conditional_switch_features.c")

  println("\nSuperC (V)AST (original data structure):")
  println(superCAstDotString)
  println("\nSuperC (V)AST (translated to JOERN VAST data structure):")
  println(superCJoernAstDotString)

  println(
    """
      |
      |=== Conditional SWITCH extras checklist (Joern, method `test`) ===
      |
      |1) Switch cond: SWITCH condition is CHOICE IDENTIFIER x / y under USE_X
      |2) Case label: CHOICE JUMP_TARGET case 0 vs case 1 under CASE_ZERO
      |   (helper(x) should stay outside that CHOICE if SuperC shares the body)
      |3) Optional case: CHOICE wrapping case 1 + helper(1) under HAS_ONE
      |
      |""".stripMargin
  )
}
