package io.joern.c2cpg.variability.vast

import io.joern.c2cpg.testfixtures.C2CpgSuite
import io.joern.c2cpg.variability.util.TestUtil.generateVASTDot

class testFunctionCall extends C2CpgSuite(withOssDataflow = true) {

  // Task 16: plain calls, conditional function name/args, elif + nested conditionals in args (whiteboard helper2 case).
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
      |  helper2(
      |    "z",
      |#if USE_ARG_A1
      |    "a", "2",
      |#elif USE_ARG_B2
      |    "c",
      |#if USE_D3
      |    "d",
      |#else
      |    "e",
      |#endif
      |#endif
      |    "g");
      |  baz(a, b);
      |}
      |""".replace("\r", "").stripMargin

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
      |2) Multi-arg call (baz — not bar, bar is only in #ifdef CHOICE):
      |   NAME=baz, CODE=baz(a, b), children IDENTIFIER a and IDENTIFIER b
      |
      |3) Call in init:
      |   <operator>.assignment -> RHS CALL helper(a)
      |
      |4) Conditional function name — search CHOICE first:
      |   branch 1: CALL NAME=foo, CODE=foo(a)
      |   branch 2: CALL NAME=bar, CODE=bar(a)
      |
      |5) Single conditional argument (helper):
      |   CALL helper — one arg is CHOICE with IDENTIFIER a / IDENTIFIER b
      |
      |6) helper2 fixed args — every branch must keep "z" first and "g" last:
      |   CALL NAME=helper2, search CODE containing "z" and "g"
      |
      |7) helper2 elif branch (USE_ARG_A1 vs USE_ARG_B2):
      |   CHOICE under helper2 args: branch A has "a","2"; branch B has "c" + nested choice
      |
      |8) helper2 nested conditional (USE_D3 inside elif-B):
      |   Inner CHOICE under elif-B: "d" vs "e" (whiteboard: if{c, if{d}else{e}})
      |
      |""".stripMargin
  )
}
