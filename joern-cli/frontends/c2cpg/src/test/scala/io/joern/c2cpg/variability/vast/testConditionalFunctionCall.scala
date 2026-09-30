package io.joern.c2cpg.variability.vast

import io.joern.c2cpg.testfixtures.C2CpgSuite
import io.joern.c2cpg.variability.util.TestUtil.generateVASTDot

/** Fynn: FunctionCall + Conditionals — name, args, count, order, 0/1/n params. */
class testConditionalFunctionCall extends C2CpgSuite(withOssDataflow = true) {

  val cCode: String =
    """
      |void foo(int a, int b) {}
      |void bar(int a, int b) {}
      |void baz() {}
      |
      |void test(int a, int b) {
      |  /* 1) function name variable */
      |#ifdef USE_FOO
      |  foo(a, b);
      |#else
      |  bar(a, b);
      |#endif
      |
      |  /* 2) parameter identity / name variable */
      |  foo(
      |#ifdef USE_A
      |    a
      |#else
      |    b
      |#endif
      |  );
      |
      |  /* 3) number of parameters variable (1 vs 2) */
      |  foo(a
      |#ifdef TWO_ARGS
      |    , b
      |#endif
      |  );
      |
      |  /* 4) parameter order variable */
      |#ifdef SWAP
      |  foo(b, a);
      |#else
      |  foo(a, b);
      |#endif
      |
      |  /* 5) edge: 0 / 1 / many parameters */
      |#ifdef NONE
      |  baz();
      |#elif ONE
      |  foo(a);
      |#else
      |  foo(a, b);
      |#endif
      |}
      |""".replace("\r", "").stripMargin

  val (superCAstDotString, superCJoernAstDotString) = generateVASTDot(cCode, "test_conditional_function_call.c")

  println("\nSuperC (V)AST (original data structure):")
  println(superCAstDotString)
  println("\nSuperC (V)AST (translated to JOERN VAST data structure):")
  println(superCJoernAstDotString)

  println(
    """
      |
      |=== Conditional FunctionCall checklist (Joern, method `test`) ===
      |
      |1) Name: CHOICE — CALL foo(a,b) vs CALL bar(a,b) under USE_FOO
      |2) Arg identity: CALL foo with CHOICE IDENTIFIER a / b under USE_A
      |3) Arg count: CALL foo — second arg only under TWO_ARGS (CHOICE or optional arg)
      |4) Arg order: CHOICE — foo(b,a) vs foo(a,b) under SWAP
      |5) 0/1/n: CHOICE — baz() / foo(a) / foo(a,b) under NONE / ONE / else
      |
      |Note: IDENTIFIER quality may still be limited (Fynn: variable-identifier WIP).
      |
      |""".stripMargin
  )
}
