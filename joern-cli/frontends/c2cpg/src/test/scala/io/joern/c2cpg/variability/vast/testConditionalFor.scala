package io.joern.c2cpg.variability.vast

import io.joern.c2cpg.testfixtures.C2CpgSuite
import io.joern.c2cpg.variability.util.TestUtil.generateVASTDot

/** Task 20 — Fynn's conditional for: #if on init / cond / update / body. */
class testConditionalFor extends C2CpgSuite(withOssDataflow = true) {

  val cCode: String =
    """
      |#if m0
      |  int ii = 0
      |#else
      |  long ii = 1
      |#endif
      |  ;
      |
      |void i() {
      |  for (
      |#if m0
      |  int i = 0
      |#else
      |  long i = 1
      |#endif
      |  ;
      |#if m1
      |  i < 42
      |#else
      |  i < 10
      |#endif
      |  ;
      |#if m2
      |  i++
      |#else
      |  i += 2
      |#endif
      |  ) {
      |#if m3
      |    print(i);
      |#else
      |    println(i);
      |#endif
      |  }
      |}
      |
      |
      |void i2() {
      |  for (
      |#if m0
      |  int i = 0
      |#else
      |  long i = 1
      |#endif
      |  ;
      |#if m1
      |  i < 42
      |#else
      |  i <= 10
      |#endif
      |  ;
      |#if m2
      |  i++
      |#else
      |  i += 2
      |#endif
      |  ) {
      |#if m3
      |    print(i);
      |    print("w");
      |#else
      |    println(i);
      |    i = 42;
      |#endif
      |  }
      |}
      |""".stripMargin

  val (superCAstDotString, superCJoernAstDotString) = generateVASTDot(cCode, "test_c_file.c")

  println("\nSuperC (V)AST (original data structure):")
  println(superCAstDotString)
  println("\nSuperC (V)AST (translated to JOERN VAST data structure):")
  println(superCJoernAstDotString)

  println(
    """
      |
      |=== Task 20 conditional-for checklist (Joern, method `i`) ===
      |
      |1) CONTROL_STRUCTURE TYPE=FOR still present
      |
      |2) Init (m0): CHOICE — int i = 0  vs  long i = 1
      |
      |3) Condition (m1): CHOICE — i < 42  vs  i < 10
      |
      |4) Update (m2): CHOICE — i++  vs  i += 2
      |
      |5) Body (m3): CHOICE — print(i)  vs  println(i)
      |
      |SuperC original: ForStatement children may each be Conditional
      |  [0] Conditional(Declaration)  [1] Conditional(Relational)
      |  [2] Conditional(Increment/Assign)  [3] CompoundStatement with Conditional
      |
      |""".stripMargin
  )
}
