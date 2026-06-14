package io.joern.c2cpg.variability.vast

import io.joern.c2cpg.testfixtures.{C2CpgSuite, CDefaultTestCpg}
import io.joern.c2cpg.variability.util.TestUtil.generateVASTDot
import io.shiftleft.codepropertygraph.generated.nodes
import io.shiftleft.codepropertygraph.generated.nodes.Method
import io.shiftleft.semanticcpg.dotgenerator.DotAstGenerator

class testConditionalMacro extends C2CpgSuite(withOssDataflow = true) {
  val cCode: String
  =
  """
    | #ifdef MACRO1:
    | int a;
    | #elif MACRO2:
    | int b;
    | #else:
    | int c;
    | #if MACRO3:
    | int d;
    | #endif
    | #endif
    |
    | #ifdef MACRO4 && MACRO5
    | int e;
    | #endif
    | int f;
    | int foo(int g) {
    |   int h = (int) g;
    |   return h;
    | }
    |
    |
    | // #define DEBUG_LEVEL 2
    | // #define ENABLE_LOGS  2
    |
    | // Logical expression combining comparison and AND
    | #if (DEBUG_LEVEL > 1) && (ENABLE_LOGS == 1)
    |   #define LOG_MSG(msg) printf("DEBUG: %s\n", msg)
    |   float w1 = 42;
    | #else
    |   #define LOG_MSG(msg) // Becomes a blank line, stripping it from build
    |   float w2 = 41;
    | #endif
    |
    | int main() {
    |   LOG_MSG("System initialized."); // Compiles only if conditions match
    |   int aaa;
    |   return 0;
    | }
    |""".stripMargin
  val cCpg: CDefaultTestCpg = code(cCode)
  val cTraversal: Iterator[Method] = cCpg.graph._nodes(25).asInstanceOf[Iterator[nodes.Method]]
  val cAstDotString: Iterator[String] = DotAstGenerator.dotAst(cTraversal, extended_view = true)
  println("Standard Joern C AST:")
  println(cAstDotString.mkString)


  val (superCAstDotString: String, superCJoernAstDotString: String) = generateVASTDot(cCode)
  println("\n\n\nSuperC (V)AST (original data structure):")
  println(superCAstDotString)

  println("\n\n\nSuperC (V)AST (translated to JOERN VAST data structure):")
  println(superCJoernAstDotString)
}
