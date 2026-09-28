package io.joern.c2cpg.variability.vast

import io.joern.c2cpg.testfixtures.C2CpgSuite
import io.joern.c2cpg.variability.util.TestUtil.generateVASTDot
import io.joern.dataflowengineoss.dotgenerator.DotCpg14Generator
import io.joern.x2cpg.testfixtures.TestCpg
import io.shiftleft.codepropertygraph.generated.nodes
import io.shiftleft.codepropertygraph.generated.nodes.Method
import io.shiftleft.semanticcpg.dotgenerator.DotAstGenerator

class testCodeBlocks extends C2CpgSuite(withOssDataflow = true) {
  val cFilename: String = "test_c_file.c"
  val cCode: String =
    """
      | int main() {
      |   int c = 0;
      |   {
      |     int a = 1;
      |     a = 2;
      |     char c = 'w';
      |     c = 'e';
      |   }
      |   {
      |     int b = 3;
      |     b = 4;
      |     float c = 0.5;
      |     c = 1.4;
      |   }
      |   {}
      |   if (c == 2) {
      |     foo(c);
      |     short c = 3;
      |     c = 4;
      |   } else if (c == 3) {
      |     bar(c);
      |     long c = -1;
      |     c = 2;
      |   } else {
      |     bez(c);
      |     unsigned int c = 6;
      |     c = 9;
      |   }
      |   for (int q = 0; q < 10; q++) {
      |     foo(q);
      |     bar(c);
      |     unsigned long c = 0;
      |     baz(c);
      |   }
      |   while (c == 10) {
      |     foo(c);
      |     unsigned short c = 7;
      |     bar(c);
      |   }
      |   do {
      |     foo(c);
      |     float c = 8;
      |     bar(c);
      |   } while (c == 11);
      |
      |   switch (c) {
      |     case 1:
      |       printf("Monday\n");
      |       break;
      |     case 2:
      |       printf("Tuesday\n");
      |       break;
      |     case 3:
      |       c = 0;
      |       printf("Wednesday\n");
      |
      |     case 4:
      |       c = 0;
      |       printf("Thursday\n");
      |       break;
      |     case 5:
      |       c = 0;
      |       printf("friday\n");
      |       break;
      |
      |     case 6:
      |     case 7:
      |       c = 0;
      |       printf("weekend\n");
      |       break;
      |
      |     default:
      |       c = 0;
      |       printf("no day\n");
      |       break;
      |   }
      |   return c;
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
