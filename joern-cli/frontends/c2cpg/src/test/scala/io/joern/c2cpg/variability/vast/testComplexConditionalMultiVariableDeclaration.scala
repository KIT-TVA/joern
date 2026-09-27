package io.joern.c2cpg.variability.vast

import io.joern.c2cpg.testfixtures.C2CpgSuite
import io.joern.c2cpg.variability.util.TestUtil.generateVASTDot
import io.joern.x2cpg.testfixtures.TestCpg
import io.shiftleft.codepropertygraph.generated.nodes
import io.shiftleft.codepropertygraph.generated.nodes.Method
import io.shiftleft.semanticcpg.dotgenerator.DotAstGenerator


class testComplexConditionalMultiVariableDeclaration extends C2CpgSuite(withOssDataflow = true) {
  val cFilename: String = "test_c_file.c"
  val cCode: String =
    """
      | int a, b;
      | char c, d = 'w';
      | long l = 6, k;
      | long m = 4, n = 3;
      | char x, y, z;
      | int o = 2, p = 3, q = 4;
      | long t = a + b + 3;
      | enum color c1, c2, c3 = GREEN, **c4, c5[2][3][4], ****c6[5][6][7];
      | struct Pointer p1, p2, p3, **p4, p5[2][3][4], ****p6[5][6][7];
      |
      | #ifdef M0
      | long
      | #else
      | int
      | #endif
      | #ifdef M1
      | r
      | #else
      | s
      | #endif
      | #ifdef M2
      | = 2
      | #endif
      | ;
      | char f
      | #ifdef M3
      | , g = 6
      | #endif
      | ;
      | long w =
      | #ifdef M4:
      | 3 + 5
      | #else
      | 4 - 6
      | #endif
      | ;
      | long v
      | #ifdef M5
      | = 8 * 9;
      | #endif
      | ;
      | char c1,
      | #ifdef MC00
      | c2,
      | #else
      | c3,
      | #endif
      | c4;
      | void i() {
      |   a = 49;
      |   b = 3 + a;
      | }
      |""".stripMargin
  val cCpg: TestCpg = code(cCode, cFilename)
  val cTraversal: Iterator[Method] = cCpg.graph._nodes(25).asInstanceOf[Iterator[nodes.Method]]
  val cAstDotString: Iterator[String] = DotAstGenerator.dotAst(cTraversal, extendedView=true, withColoring=true)
  println("Standard Joern C AST:")
  println(cAstDotString.mkString)


  val (superCAstDotString, superCJoernAstDotString) = generateVASTDot(cCode, cFilename)
  println("\n\n\nSuperC (V)AST (original data structure):")
  println(superCAstDotString)

  println("\n\n\nSuperC (V)AST (translated to JOERN VAST data structure):")
  println(superCJoernAstDotString)
}
