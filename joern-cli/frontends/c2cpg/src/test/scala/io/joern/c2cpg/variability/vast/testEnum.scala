package io.joern.c2cpg.variability.vast

import io.joern.c2cpg.testfixtures.C2CpgSuite
import io.joern.c2cpg.variability.util.TestUtil.generateVASTDot

/** Task 7: plain enum → TYPE_DECL + MEMBER. */
class testEnum extends C2CpgSuite(withOssDataflow = true) {

  val cCode: String =
    """
      |enum color {
      |  red,
      |  yellow,
      |  green = 20,
      |  blue
      |};
      |
      |void test(enum color c) {
      |  int x = green;
      |}
      |""".stripMargin

  val (superCAstDotString, superCJoernAstDotString) = generateVASTDot(cCode, "test_enum.c")

  println("\nSuperC (V)AST (original data structure):")
  println(superCAstDotString)
  println("\nSuperC (V)AST (translated to JOERN VAST data structure):")
  println(superCJoernAstDotString)

  println(
    """
      |
      |=== Task 7 enum checklist (Joern VAST) ===
      |
      |1) TYPE_DECL name=color (CODE contains "enum color")
      |2) MEMBER children: red, yellow, green, blue
      |3) Optional: CALL assignment for green = 20
      |4) SuperC original: search Enum / Enumeration nodes
      |
      |""".stripMargin
  )
}
