package io.joern.c2cpg.variability.vast

import io.joern.c2cpg.testfixtures.C2CpgSuite
import io.joern.c2cpg.variability.util.TestUtil.generateVASTDot
import io.joern.x2cpg.testfixtures.TestCpg
import io.shiftleft.codepropertygraph.generated.nodes
import io.shiftleft.codepropertygraph.generated.nodes.Method
import io.shiftleft.semanticcpg.dotgenerator.DotAstGenerator

/** Task 7: plain enum → TYPE_DECL + MEMBER. */
class testComplexFunctionParametertypes extends C2CpgSuite(withOssDataflow = true) {

  val cCode: String =
    """
      |
      |void test(enum color c1, enum color **c2, enum color **c3[2][3], int a1, int **a2[4][5], struct Point p1, struct Point **p2) {
      |  int x = green;
      |}
      |
      |void ttt(const int i, extern float f, const unsigned int ui, extern unsigned long ul, const enum color cc, extern enum color ec, unsigned int ui2) {}
      |
      |void a(
      |#if M0
      |enum
      |#if E0
      |color
      |#else
      |color2
      |#endif
      |#elif M1
      |struct
      |#if S0
      |Point
      |#else
      |P
      |#endif
      |#elif M2
      |Pointer
      |#else
      |float
      |#endif
      |ppp,
      |const long www,
      |const enum color ccc,
      |unsigned char char1,
      |#if W0
      |const
      |#else
      |unsigned
      |#endif
      |#if B0
      |enum color
      |#else
      |float
      |#endif
      |q,
      |const
      |#if l0
      |float
      |#else
      |long
      |#endif
      |ddddd,
      |int a) {}
      |
      |void www(
      |#if T0
      |const
      |#else
      |external
      |#endif
      |#if T1
      |unsigned
      |#elif T2
      |signed
      |#endif
      |int i, int w) {}
      |
      |""".stripMargin

  val cCpg: TestCpg = code(cCode, "Test.c")
  val cTraversal: Iterator[Method] = cCpg.graph._nodes(25).asInstanceOf[Iterator[nodes.Method]]
  val cAstDotString: Iterator[String] = DotAstGenerator.dotAst(cTraversal, extendedView = true, withColoring = true)
  println("Standard Joern C AST:")
  println(cAstDotString.mkString)

  val (superCAstDotString, superCJoernAstDotString) = generateVASTDot(cCode, "test_enum.c")

  println("\nSuperC (V)AST (original data structure):")
  println(superCAstDotString)
  println("\nSuperC (V)AST (translated to JOERN VAST data structure):")
  println(superCJoernAstDotString)
  
}
