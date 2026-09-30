package io.joern.c2cpg.variability.vast

import io.joern.c2cpg.testfixtures.C2CpgSuite
import io.joern.c2cpg.variability.util.TestUtil.generateVASTDot

/**
 * Fynn: IF + Conditionals — condition, then/else body, whole statement.
 *
 * HAS_IF lives in its own function so a presence-CHOICE cannot wrap earlier IFs
 * (known SuperC / presence-mixing quirk when the last stmt is optional).
 */
class testConditionalIf extends C2CpgSuite(withOssDataflow = true) {

  val cCode: String =
    """
      |void helper(int x) {}
      |
      |void test_cond_then_else(int x, int y) {
      |  /* 1) condition variable — left operand of `>` is #ifdef */
      |  if (
      |#ifdef USE_X
      |    x
      |#else
      |    y
      |#endif
      |    > 0) {
      |    helper(x);
      |  }
      |
      |  /* 2) then-body variable */
      |  if (x > 0) {
      |#ifdef THEN_X
      |    helper(x);
      |#else
      |    helper(y);
      |#endif
      |  }
      |
      |  /* 3) else-body variable */
      |  if (x > 0) {
      |    helper(x);
      |  } else {
      |#ifdef ELSE_Y
      |    helper(y);
      |#else
      |    helper(0);
      |#endif
      |  }
      |}
      |
      |void test_presence(int x) {
      |  /* 4) whole if present or not — isolated method */
      |#ifdef HAS_IF
      |  if (x > 0) {
      |    helper(x);
      |  }
      |#endif
      |}
      |""".replace("\r", "").stripMargin

  val (superCAstDotString, superCJoernAstDotString) = generateVASTDot(cCode, "test_conditional_if.c")

  println("\nSuperC (V)AST (original data structure):")
  println(superCAstDotString)
  println("\nSuperC (V)AST (translated to JOERN VAST data structure):")
  println(superCJoernAstDotString)

  println(
    """
      |
      |=== Conditional IF checklist (Joern) ===
      |
      |method test_cond_then_else:
      |1) Condition: IF condition CALL `>` with CHOICE IDENTIFIER x / y under USE_X
      |2) Then body: IF then-BLOCK has CHOICE helper(x) / helper(y) under THEN_X
      |3) Else body: ELSE BLOCK has CHOICE helper(y) / helper(0) under ELSE_Y
      |
      |method test_presence:
      |4) Whole if: CHOICE wrapping CONTROL_STRUCTURE IF under HAS_IF (expect ~1 CHOICE here)
      |
      |""".stripMargin
  )
}
