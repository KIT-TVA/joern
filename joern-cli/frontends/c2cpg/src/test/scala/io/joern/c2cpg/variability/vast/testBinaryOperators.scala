package io.joern.c2cpg.variability.vast

import io.joern.c2cpg.testfixtures.C2CpgSuite
import io.joern.c2cpg.variability.util.TestUtil.generateVASTDot

class testBinaryOperators extends C2CpgSuite(withOssDataflow = true) {

  val cCode: String =
    """
      |int foo(int a, int b) {
      |  int m = a * b;
      |  int d = a / b;
      |  int r = a % b;
      |  int sum = a + b;
      |  int diff = a - b;
      |  int shl = a << 2;
      |  int shr = b >> 1;
      |  int lt = a < b;
      |  int gt = a > b;
      |  int le = a <= b;
      |  int ge = a >= b;
      |  int eq = a == b;
      |  int ne = a != b;
      |  int band = a & b;
      |  int bxor = a ^ b;
      |  int bor = a | b;
      |  int land = a && b;
      |  int lor = a || b;
      |  int copy = a;
      |  a += b;
      |  int casted = (int) a;
      |  return sum;
      |}
      |""".stripMargin

  val (superCAstDotString, superCJoernAstDotString) = generateVASTDot(cCode, "test_binary_operators.c")

  println("\nSuperC (V)AST (original data structure):")
  println(superCAstDotString)
  println("\nSuperC (V)AST (translated to JOERN VAST data structure):")
  println(superCJoernAstDotString)

  println(
    """
      |
      |=== Task 2 checklist (search in Joern dot output) ===
      |operator.multiplication    *
      |operator.division          /
      |operator.modulo            %
      |operator.addition          +
      |operator.subtraction       -
      |operator.shiftLeft         <<
      |operator.arithmeticShiftRight >>
      |operator.lessThan          <
      |operator.greaterThan       >
      |operator.lessEqualsThan     <=
      |operator.greaterEqualsThan  >=
      |operator.equals            ==
      |operator.notEquals         !=
      |operator.and               &
      |operator.xor               ^
      |operator.or                |
      |operator.logicalAnd        &&
      |operator.logicalOr         ||
      |operator.assignment        =   (int copy = a)
      |operator.assignmentPlus    +=  (a += b statement)
      |operator.cast              (int) a
      |Skipped: max min (GNU extension, non-standard C syntax)
      |""".stripMargin
  )
}
