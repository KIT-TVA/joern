package io.joern.c2cpg.variability.vast.demo

import io.joern.c2cpg.testfixtures.C2CpgSuite
import io.joern.c2cpg.variability.util.TestUtil.generateVASTDot
import io.joern.dataflowengineoss.dotgenerator.DotCpg14Generator
import io.joern.x2cpg.testfixtures.TestCpg
import io.shiftleft.codepropertygraph.generated.nodes
import io.shiftleft.codepropertygraph.generated.nodes.Method
import io.shiftleft.semanticcpg.dotgenerator.DotAstGenerator

class VASTConditionalVariabilitySample extends C2CpgSuite(withOssDataflow = true) {
  val cFilename: String = "conditional_variability_sample.c"
  val cCode: String =
    """
      | void foo(
      | #if M0
      | int
      | #else
      | float
      | #endif
      | #if M1
      | **
      | #if M2
      | **
      | #else
      | *
      | #endif
      | #endif
      | #if M3
      | arr1
      | #else
      | arr2
      | #endif
      | #if M4
      | [
      | #if M5
      | 4
      | #elif M6
      | 5
      | #endif
      | ][2]
      | #elif M7
      | [7]
      | #if M8
      | [8]
      | #else
      | [
      | #if M9
      | 11
      | #else
      | 12][13
      | #endif
      | ]
      | #endif
      | [10]
      | #elif M10
      | [6][9]
      | #endif
      | ) {}
      |
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
