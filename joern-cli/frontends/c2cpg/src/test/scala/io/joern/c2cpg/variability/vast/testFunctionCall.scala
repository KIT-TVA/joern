package io.joern.c2cpg.variability.vast

import io.joern.c2cpg.testfixtures.C2CpgSuite
import io.joern.c2cpg.variability.util.TestUtil.generateVASTDot

class testFunctionCall extends C2CpgSuite(withOssDataflow = true) {

  val cCode: String =
    """
      |void helper(int x) {}
      |
      |void test(int a, int b) {
      |  helper(a);
      |  int r = helper(a);
      |  helper(
      |#ifdef USE_ARG_A
      |a
      |#else
      |b
      |#endif
      |);
      |#ifdef USE_FOO
      |foo
      |#else
      |bar
      |#endif
      |(a);
      |  baz(a, b);
      |}
      |""".stripMargin

  val (superCAstDotString, superCJoernAstDotString) = generateVASTDot(cCode, "test_function_call.c")

  println("\nSuperC (V)AST (original data structure):")
  println(superCAstDotString)
  println("\nSuperC (V)AST (translated to JOERN VAST data structure):")
  println(superCJoernAstDotString)

  println(
    """
      |
      |=== FunctionCall checklist (Joern dot, method `test`) ===
      |
      |1) Plain 1-arg call:
      |   NAME=helper, CODE=helper(a), child IDENTIFIER a
      |
      |2) Multi-arg call (NOT bar — use baz to avoid clash with #else bar):
      |   NAME=baz, CODE=baz(a, b), children IDENTIFIER a and IDENTIFIER b
      |
      |3) Call in init:
      |   <operator>.assignment -> RHS CALL helper(a)
      |
      |4) Conditional function name — search CHOICE first, then bar:
      |   CONTROL_STRUCTURE TYPE=CHOICE
      |   branch 1: CALL NAME=foo, CODE=foo(a), child IDENTIFIER a
      |   branch 2: CALL NAME=bar, CODE=bar(a), child IDENTIFIER a
      |
      |5) Conditional argument:
      |   CALL helper with arg = CHOICE or IDENTIFIER a / IDENTIFIER b under branches
      |
      |""".stripMargin
  )
}
