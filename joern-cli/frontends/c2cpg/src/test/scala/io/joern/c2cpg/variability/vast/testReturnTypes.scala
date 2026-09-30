package io.joern.c2cpg.variability.vast

import io.joern.c2cpg.testfixtures.C2CpgSuite
import io.joern.c2cpg.variability.util.TestUtil.generateVASTDot
import io.joern.dataflowengineoss.dotgenerator.DotCpg14Generator
import io.joern.x2cpg.testfixtures.TestCpg
import io.shiftleft.codepropertygraph.generated.nodes.Method
import io.shiftleft.codepropertygraph.generated.nodes
import io.shiftleft.semanticcpg.dotgenerator.DotAstGenerator

class testReturnTypes extends C2CpgSuite(withOssDataflow = true) {
    val cFilename: String = "test_c_file.c"
    val cCode: String =
      """
        |
        | int returnConst(int size) {
        |   size = 1;
        |   return 42;
        | }
        |
        | int returnVariable(int size) {
        |   size = 2;
        |   return size;
        | }
        |
        | int returnExpression(int size) {
        |   size = 3;
        |   return 30 + size;
        | }
        |
        | int returnExpression(int size) {
        |   size = 4;
        |   return returnConst(size);
        | }
        |
        | void returnBreak(int size) {
        |   size = 5;
        |   return;
        | }
        |
        |""".stripMargin
    val cCpg: TestCpg = code(cCode, cFilename)
    val cTraversal: Iterator[Method] = cCpg.graph._nodes(25).asInstanceOf[Iterator[nodes.Method]]
    val cAstDotString: Iterator[String] = DotCpg14Generator.toDotCpg14(cTraversal, extendedView=true, withColoring=true,
                                                                       forceTreeStructure=true)
    println("Standard Joern C AST:")
    println(cAstDotString.mkString)


    val (superCAstDotString, superCJoernAstDotString) = generateVASTDot(cCode, cFilename)
    println("\n\n\nSuperC (V)AST (original data structure):")
    println(superCAstDotString)

    println("\n\n\nSuperC (V)AST (translated to JOERN VAST data structure):")
    println(superCJoernAstDotString)
  }
