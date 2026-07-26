package io.joern.c2cpg.variability.vast

import io.joern.c2cpg.testfixtures.C2CpgSuite
import io.joern.c2cpg.variability.util.TestUtil.generateVASTDot

class testUnaryOperators extends C2CpgSuite(withOssDataflow = true) {

  val cCode: String =
    """
      |int foo(int x, int *p, int flag) {
      |  int a = ++x;
      |  int b = --x;
      |  int c = +x;
      |  int d = -x;
      |  int e = *p;
      |  int *addr = &x;
      |  int f = !flag;
      |  int g = ~flag;
      |  int h = sizeof(x);
      |  int grouped = (x);
      |  x++;
      |  x--;
      |  float f1 = (float) h;
      |  return d;
      |}
      |""".stripMargin

  val (superCAstDotString, superCJoernAstDotString) = generateVASTDot(cCode, "test_unary_operators.c")

  println("\nSuperC (V)AST (original data structure):")
  println(superCAstDotString)
  println("\nSuperC (V)AST (translated to JOERN VAST data structure):")
  println(superCJoernAstDotString)

  println(
    """
      |
      |=== Task 1 checklist (search in Joern dot output) ===
      |<operator>.preIncrement      ++x
      |<operator>.preDecrement      --x
      |<operator>.plus              +x
      |<operator>.minus             -x
      |<operator>.indirection       *p
      |<operator>.addressOf         &x
      |<operator>.logicalNot        !flag
      |<operator>.not               ~flag
      |<operator>.sizeOf            sizeof(x)
      |Skipped: typeof(x)           SuperC parser does not accept typeof (GCC extension)
      |<operator>.postIncrement     x++
      |<operator>.postDecrement     x--
      |Parenthesized (x)              grouped = (x)  — inner IDENTIFIER x, no dummy BLOCK
      |<operator>.cast              (float) h  (Task 2 Cast converter, no dummy BLOCK after f1 =)
      |""".stripMargin
  )
}
