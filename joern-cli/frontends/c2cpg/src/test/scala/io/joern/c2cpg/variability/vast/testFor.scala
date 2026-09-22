package io.joern.c2cpg.variability.vast

import io.joern.c2cpg.testfixtures.C2CpgSuite
import io.joern.c2cpg.variability.util.TestUtil.generateVASTDot
import io.joern.x2cpg.testfixtures.TestCpg
import io.shiftleft.codepropertygraph.generated.nodes
import io.shiftleft.codepropertygraph.generated.nodes.Method
import io.shiftleft.semanticcpg.dotgenerator.DotAstGenerator

class testFor extends C2CpgSuite(withOssDataflow = true) {

  val cCode: String =
    """
      |void i() {
      |  for (int i = 0; i < 42; i++) {
      |    print(i);
      |  }
      |}
      |""".stripMargin

  val cCpg: TestCpg = code(cCode, "test_c_file.c")
  val cTraversal: Iterator[Method] = cCpg.graph._nodes(25).asInstanceOf[Iterator[nodes.Method]]
  val cAstDotString: Iterator[String] = DotAstGenerator.dotAst(cTraversal, extended_view = true)
  println("Standard Joern C AST:")
  println(cAstDotString.mkString)

  val (superCAstDotString, superCJoernAstDotString) = generateVASTDot(cCode, "test_c_file.c")

  println("\nSuperC (V)AST (original data structure):")
  println(superCAstDotString)
  println("\nSuperC (V)AST (translated to JOERN VAST data structure):")
  println(superCJoernAstDotString)

  println(
    """
      |
      |=== Task 20 for checklist (Joern dot, method `i`) — Fynn SuperC layout ===
      |
      |SuperC ForStatement children:
      |  [0] Declaration     int i = 0
      |  [1] RelationalExpr  i < 42
      |  [2] Increment       i++
      |  [3] CompoundStatement { print(i); }
      |
      |Joern expect:
      |1) CONTROL_STRUCTURE TYPE=FOR
      |2) init: LOCAL i + assignment i = 0 (or similar)
      |3) condition: <operator>.lessThan  i < 42
      |4) update: <operator>.postIncrement  i++
      |5) body BLOCK: CALL print(i)
      |
      |""".stripMargin
  )
}
