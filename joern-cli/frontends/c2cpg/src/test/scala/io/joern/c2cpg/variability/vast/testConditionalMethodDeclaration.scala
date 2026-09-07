package io.joern.c2cpg.variability.vast

import io.joern.c2cpg.testfixtures.C2CpgSuite
import io.joern.c2cpg.variability.util.TestUtil.generateVASTDot
import io.joern.dataflowengineoss.dotgenerator.DotCpg14Generator
import io.joern.x2cpg.testfixtures.TestCpg
import io.shiftleft.codepropertygraph.generated.nodes.Method
import io.shiftleft.codepropertygraph.generated.nodes
import io.shiftleft.semanticcpg.dotgenerator.DotAstGenerator

class testConditionalMethodDeclaration extends C2CpgSuite(withOssDataflow = true) {
  val cFilename: String = "test_c_file.c"
  val cCode: String =
    """
      |
      | // Method A: Unsized array syntax (Most common for readability)
      | void printArrayA(int arr[], int size) {
      |   size = 1;
      | }
      |
      |  // Method B: Pointer syntax (What the compiler actually sees)
      | void printArrayB(int *arr, int size) {
      |   size = 2;
      | }
      |
      | // Method C: Pointer syntax (What the compiler actually sees)
      | void printArrayC(int **arr, int size) {
      |   size = 3;
      | }
      |
      | // Method D: Pointer syntax (What the compiler actually sees)
      | void printArrayD(int **arr[], int size) {
      |   size = 4;
      | }
      |
      | // Method E: Pointer syntax (What the compiler actually sees)
      | void printArrayE(int **arr[4][6], int size) {
      |   size = 5;
      | }
      |
      | // Method F: Pointer syntax (What the compiler actually sees)
      | void printArrayF(int **arr[4][6][9], int size) {
      |   size = 6;
      | }
      |
      | // Method G: Sized array syntax (The size 5 is ignored by the compiler!)
      | void printArrayG(int arr[5], int size) {
      |   size = 7;
      | }
      |
      | // Method H: Pointer syntax (What the compiler actually sees)
      | void printArrayH(int arr[4][6][9], int size) {
      |   size = 8;
      | }
      |
      | // Method I: Pointer syntax (What the compiler actually sees)
      | void printArrayI(
      | #if N0
      | int
      | #else
      | float
      | #endif
      | #if N1
      | **
      | #if N2
      | **
      | #else
      | *
      | #endif
      | #endif
      | //#if N3
      | arr
      | //#else
      | //arr2
      | //#endif
      | #if N4
      | [
      | #if N5
      | 4
      | #elif N6
      | 5
      | #endif
      | ][2]
      | #elif N7
      | [6][9]
      | #endif
      | ,
      | #if G0:
      | int size
      | #else
      | float www
      | #endif
      | ) {
      |   size = 8;
      | }
      |
      | #if M0
      | void
      | #else
      | float
      | #endif
      | a() {
      |   a = 49;
      |   if (a == 2) {
      |     print("a == 2");
      |   } else {
      |     print("a != 2");
      |   }
      |   b = 3 + a;
      |   int w = 3 + 4;
      |   int v = 1 * 5;
      |   #if !M0
      |   return (float) b;
      |   #endif
      | }
      |
      | int
      | #if M0
      | bTrue
      | #else
      | bFalse
      | #endif
      | (int a1) {
      |   a = 49;
      |   a = a + a1;
      |   return a;
      | }
      |
      | float c(int a,
      | # if M2
      | float b0
      | #else
      | long b1
      | #endif
      | #if M3
      | , char c0, char c1
      | #else
      | , long l0, long l1, long l2
      | #endif
      | ) {
      | float c = (float) a;
      |   return c;
      | }
      |
      |
      |""".stripMargin
  val cCpg: TestCpg = code(cCode, cFilename)
  val cTraversal: Iterator[Method] = cCpg.graph._nodes(25).asInstanceOf[Iterator[nodes.Method]]
  val cAstDotString: Iterator[String] = DotCpg14Generator.toDotCpg14(cTraversal, extendedView=true)
  // val cAstDotString: Iterator[String] = DotAstGenerator.dotAst(cTraversal, extended_view = true)
  println("Standard Joern C AST:")
  println(cAstDotString.mkString)


  val (superCAstDotString, superCJoernAstDotString) = generateVASTDot(cCode, cFilename)
  println("\n\n\nSuperC (V)AST (original data structure):")
  println(superCAstDotString)

  println("\n\n\nSuperC (V)AST (translated to JOERN VAST data structure):")
  println(superCJoernAstDotString)
}
