package io.joern.c2cpg.variability.vast

import io.joern.c2cpg.testfixtures.C2CpgSuite
import io.joern.c2cpg.variability.util.TestUtil.generateVASTDot
import io.joern.x2cpg.testfixtures.TestCpg
import io.shiftleft.codepropertygraph.generated.nodes
import io.shiftleft.codepropertygraph.generated.nodes.Method
import io.shiftleft.semanticcpg.dotgenerator.DotAstGenerator

/** Task 13: switch / case / default (plain). */
class testSwitch extends C2CpgSuite(withOssDataflow = true) {

  val cCode: String =
    """
      |void helper(int x) {}
      |
      |void test(int x) {
      |  switch (x) {
      |    case 0:
      |      helper(0);
      |      break;
      |    case 1:
      |      helper(1);
      |      break;
      |    default:
      |      helper(x);
      |      break;
      |  }
      |}
      |""".stripMargin

  val cCpg: TestCpg = code(cCode, "test_c_file.c")
  val cTraversal: Iterator[Method] = cCpg.graph._nodes(25).asInstanceOf[Iterator[nodes.Method]]
  val cAstDotString: Iterator[String] = DotAstGenerator.dotAst(cTraversal, extendedView = true, withColoring=true)
  println("Standard Joern C AST:")
  println(cAstDotString.mkString)
    
  val (superCAstDotString, superCJoernAstDotString) = generateVASTDot(cCode, "test_switch.c")

  println("\nSuperC (V)AST (original data structure):")
  println(superCAstDotString)
  println("\nSuperC (V)AST (translated to JOERN VAST data structure):")
  println(superCJoernAstDotString)

  println(
    """
      |
      |=== Task 13 switch checklist (Joern dot, method `test`) ===
      |
      |1) CONTROL_STRUCTURE TYPE=SWITCH, condition IDENTIFIER x (or similar)
      |2) body BLOCK contains:
      |   JUMP_TARGET name=case  (case 0 / case 1)
      |   JUMP_TARGET name=default
      |   CALL helper(...)
      |   CONTROL_STRUCTURE TYPE=BREAK
      |3) LINE_NUMBER / COLUMN_NUMBER from SuperC (not all 42)
      |
      |""".stripMargin
  )
}
