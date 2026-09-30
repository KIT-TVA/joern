package io.joern.c2cpg.variability.vast

import io.joern.c2cpg.testfixtures.C2CpgSuite
import io.joern.c2cpg.variability.util.TestUtil.generateVASTDot

class testMemberAccess extends C2CpgSuite(withOssDataflow = true) {

  // Use assignment statements (not int x = s.x init) so VariableDeclaration is not on the RHS path.
  val cCode: String =
    """
      |struct S {
      |  int x;
      |};
      |
      |int foo(struct S s, struct S *p) {
      |  int dotVal;
      |  int arrowVal;
      |  dotVal = s.x;
      |  arrowVal = p->x;
      |}
      |""".stripMargin

  val (superCAstDotString, superCJoernAstDotString) = generateVASTDot(cCode, "test_member_access.c")

  println("\nSuperC (V)AST (original data structure):")
  println(superCAstDotString)
  println("\nSuperC (V)AST (translated to JOERN VAST data structure):")
  println(superCJoernAstDotString)

  println(
    """
      |
      |=== Task 2 member access checklist (search in Joern dot output) ===
      |<operator>.fieldAccess           dotVal = s.x   (SuperC: DirectSelection)
      |<operator>.indirectFieldAccess   arrowVal = p->x (SuperC: IndirectSelection)
      |Each field-access CALL: IDENTIFIER (s/p) + FIELD_IDENTIFIER x
      |Also confirm VAstPatternConverterForMemberAccess is registered in VAstConverterForC
      |""".stripMargin
  )
}
