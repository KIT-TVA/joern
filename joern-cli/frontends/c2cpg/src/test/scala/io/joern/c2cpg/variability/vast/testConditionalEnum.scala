package io.joern.c2cpg.variability.vast

import io.joern.c2cpg.testfixtures.C2CpgSuite
import io.joern.c2cpg.variability.util.TestUtil.generateVASTDot

/**
 * Enum members under #ifdef + `enum color` as parameter type.
 * Parameter typing goes through Fynn's SUETypeSpecifier path (not Token.getString).
 */
class testConditionalEnum extends C2CpgSuite(withOssDataflow = true) {

  val cCode: String =
    """
      |enum color {
      |  red,
      |#ifdef HAS_YELLOW
      |  yellow,
      |#endif
      |#ifdef GREEN_EQ_20
      |  green = 20,
      |#else
      |  green,
      |#endif
      |  blue
      |};
      |
      |void test(enum color c) {
      |  int x = green;
      |}
      |""".replace("\r", "").stripMargin

  val (superCAstDotString, superCJoernAstDotString) = generateVASTDot(cCode, "test_conditional_enum.c")

  println("\nSuperC (V)AST (original data structure):")
  println(superCAstDotString)
  println("\nSuperC (V)AST (translated to JOERN VAST data structure):")
  println(superCJoernAstDotString)

  println(
    """
      |
      |=== Conditional ENUM checklist (Joern VAST) ===
      |
      |1) TYPE_DECL name=color still present
      |2) MEMBER red and blue always
      |3) yellow only under HAS_YELLOW — CHOICE around MEMBER yellow
      |4) green: CHOICE green=20 vs green under GREEN_EQ_20
      |5) METHOD test has PARAMETER with type involving enum color (no ClassCast)
      |6) IDENTIFIER green use may still be incomplete (Fynn: variable-identifier WIP)
      |
      |""".stripMargin
  )
}
