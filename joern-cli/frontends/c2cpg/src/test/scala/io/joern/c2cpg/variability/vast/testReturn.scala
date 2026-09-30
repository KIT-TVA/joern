package io.joern.c2cpg.variability.vast

import io.joern.c2cpg.astcreation.{CGlobal, VAstCreator}
import io.joern.c2cpg.testfixtures.{C2CpgSuite, CDefaultTestCpg}
import io.joern.c2cpg.variability.util.TestUtil.generateVASTDot
import io.joern.dataflowengineoss.DefaultSemantics
import io.joern.dataflowengineoss.dotgenerator.{DotCpg14Generator, DotDdgGenerator}
import io.joern.dataflowengineoss.passes.reachingdef.ReachingDefPass
import io.joern.dataflowengineoss.semanticsloader.Semantics
import io.joern.x2cpg.X2Cpg
import io.joern.x2cpg.X2Cpg.newEmptyCpg
import io.joern.x2cpg.passes.frontend.MetaDataPass
import io.joern.x2cpg.testfixtures.TestCpg
import io.shiftleft.codepropertygraph.generated.nodes.{File, Method}
import io.shiftleft.codepropertygraph.generated.{DiffGraphBuilder, Languages, nodes}
import io.shiftleft.semanticcpg.dotgenerator.DotAstGenerator
import superc.SuperC

import java.io.{File, StringReader}

class testReturn extends C2CpgSuite(withOssDataflow = true) {
  val cCode: String =
    """
      | int foo(int a){
      |   int b = a + 42;
      |   return b;
      | }
      |""".stripMargin
  val cCpg: CDefaultTestCpg = code(cCode)
  val cTraversal: Iterator[Method] = cCpg.graph._nodes(25).asInstanceOf[Iterator[nodes.Method]]
  val cAstDotString: Iterator[String] = DotAstGenerator.dotAst(cTraversal, extendedView=true, withColoring=true)
  println("Standard Joern C AST:")
  println(cAstDotString.mkString)


  val (superCAstDotString: String, superCJoernAstDotString: String) = generateVASTDot(cCode)
  println("\n\n\nSuperC (V)AST (original data structure):")
  println(superCAstDotString)

  println("\n\n\nSuperC (V)AST (translated to JOERN VAST data structure):")
  println(superCJoernAstDotString)
}

class testFuncParam extends C2CpgSuite(withOssDataflow = true) {
  val cCode: String =
    """
      | int foo(int a, int c){
      |   int b = a + 42;
      |   if (c == 2) {
      |     b = b + a;
      |   } else if (c == 3) {
      |     b = a;
      |   } else {
      |   b = 3 * b;
      |   }
      |   return b;
      | }
      |""".stripMargin
  val cCpg: CDefaultTestCpg = code(cCode)
  val cTraversal: Iterator[Method] = cCpg.graph._nodes(25).asInstanceOf[Iterator[nodes.Method]]
  val cAstDotString: Iterator[String] = DotAstGenerator.dotAst(cTraversal, extendedView=true, withColoring=true)
  println("Standard Joern C AST:")
  println(cAstDotString.mkString)


  val (superCAstDotString: String, superCJoernAstDotString: String) = generateVASTDot(cCode)
  println("\n\n\nSuperC (V)AST (original data structure):")
  println(superCAstDotString)

  println("\n\n\nSuperC (V)AST (translated to JOERN VAST data structure):")
  println(superCJoernAstDotString)
}

class testFuncSwitchCase extends C2CpgSuite(withOssDataflow = true) {
  val cCode: String =
    """
      | int foo(int a, int c){
      |   int b = a + 42;
      |   switch(c) {
      |    case 2:
      |    {
      |      b = b + a;
      |      break;
      |      }
      |    case 3:
      |      {b = a; b = b + 2;}
      |      break;
      |    default:
      |        b = 3 * b;
      |    }
      |   return b;
      | }
      |""".stripMargin
  val cCpg: CDefaultTestCpg = code(cCode)
  val cTraversal: Iterator[Method] = cCpg.graph._nodes(25).asInstanceOf[Iterator[nodes.Method]]
  val cAstDotString: Iterator[String] = DotCpg14Generator.toDotCpg14(cTraversal, extendedView=true, withColoring=true,
    forceTreeStructure=true)
  println("Standard Joern C AST:")
  println(cAstDotString.mkString)


  val (superCAstDotString: String, superCJoernAstDotString: String) = generateVASTDot(cCode)
  println("\n\n\nSuperC (V)AST (original data structure):")
  println(superCAstDotString)

  println("\n\n\nSuperC (V)AST (translated to JOERN VAST data structure):")
  println(superCJoernAstDotString)
}


class testFuncFunctionCall extends C2CpgSuite(withOssDataflow = true) {
  val cCode: String =
    """
      | int foo(int a, int c){
      |   int b = a + 42;
      |   if (c > 10) return b;
      |
      |   b = foo(b, c + 1);
      |   return b;
      | }
      |""".stripMargin
  val cCpg: CDefaultTestCpg = code(cCode)
  val cTraversal: Iterator[Method] = cCpg.graph._nodes(25).asInstanceOf[Iterator[nodes.Method]]
  val cAstDotString: Iterator[String] = DotAstGenerator.dotAst(cTraversal, extendedView=true, withColoring=true)
  println("Standard Joern C AST:")
  println(cAstDotString.mkString)


  val (superCAstDotString: String, superCJoernAstDotString: String) = generateVASTDot(cCode)
  println("\n\n\nSuperC (V)AST (original data structure):")
  println(superCAstDotString)

  println("\n\n\nSuperC (V)AST (translated to JOERN VAST data structure):")
  println(superCJoernAstDotString)
}

class testEnumInitial extends C2CpgSuite(withOssDataflow = true) {
  val cCode: String =
    """
      | enum Woche {MON, DIE, MIT, DON, FRE, SAM, SON};
      |
      | int main() {
      |   // Variable vom Typ enum Woche deklarieren
      |   enum Woche heute = MIT;
      |
      |   // Gibt 2 aus (da MIT der dritte Wert ist, beginnend bei 0)
      |   printf("Tag Nummer: %d\n", heute);
      |
      |   if (heute == MIT) {
      |     printf("Es ist Mittwoch!\n");
      |   }
      |   return 0;
      | }
      |""".stripMargin
  val cCpg: CDefaultTestCpg = code(cCode)
  val cTraversal: Iterator[Method] = cCpg.graph._nodes(25).asInstanceOf[Iterator[nodes.Method]]
  val cAstDotString: Iterator[String] = DotAstGenerator.dotAst(cTraversal, extendedView=true, withColoring=true)
  println("Standard Joern C AST:")
  println(cAstDotString.mkString)


  val (superCAstDotString: String, superCJoernAstDotString: String) = generateVASTDot(cCode)
  println("\n\n\nSuperC (V)AST (original data structure):")
  println(superCAstDotString)

  println("\n\n\nSuperC (V)AST (translated to JOERN VAST data structure):")
  println(superCJoernAstDotString)
}

// Slögt aktuell aufgund der Fehlenden Implementierung fehl.
class testMacroFunctionDefinition extends C2CpgSuite(withOssDataflow = true) {
  val cCode: String =
    """
      | #ifdef MACRO
      | int main() {
      |   printf("MACRO set!\n");
      |   return 0;
      | }
      | #else
      | int main() {
      |   printf("MACRO not set!\n");
      |   return 0;
      | }
      | #endif
      |""".stripMargin
  val cCpg: CDefaultTestCpg = code(cCode)
  val cTraversal: Iterator[Method] = cCpg.graph._nodes(25).asInstanceOf[Iterator[nodes.Method]]
  val cAstDotString: Iterator[String] = DotAstGenerator.dotAst(cTraversal, extendedView=true, withColoring=true)
  println("Standard Joern C AST:")
  println(cAstDotString.mkString)


  val (superCAstDotString: String, superCJoernAstDotString: String) = generateVASTDot(cCode)
  println("\n\n\nSuperC (V)AST (original data structure):")
  println(superCAstDotString)

  println("\n\n\nSuperC (V)AST (translated to JOERN VAST data structure):")
  println(superCJoernAstDotString)
}

//Schlägt aufrung der Fehlenden Implementuerng Fehl
class testMacroFunctionDefinitionPARTIAL extends C2CpgSuite(withOssDataflow = true) {
  val cCode: String =
    """
      |
      | int main() {
      | #ifdef MACRO
      |   printf("MACRO set!\n");
      | #else
      |   printf("MACRO not set!\n");
      | #endif
      |   return 0;
      | }
      |""".stripMargin
  val cCpg: CDefaultTestCpg = code(cCode)
  val cTraversal: Iterator[Method] = cCpg.graph._nodes(25).asInstanceOf[Iterator[nodes.Method]]
  val cAstDotString: Iterator[String] = DotAstGenerator.dotAst(cTraversal, extendedView=true, withColoring=true)
  println("Standard Joern C AST:")
  println(cAstDotString.mkString)


  val (superCAstDotString: String, superCJoernAstDotString: String) = generateVASTDot(cCode)
  println("\n\n\nSuperC (V)AST (original data structure):")
  println(superCAstDotString)

  println("\n\n\nSuperC (V)AST (translated to JOERN VAST data structure):")
  println(superCJoernAstDotString)
}

//Schlägt aufrung der Fehlenden Implementuerng Fehl
class testMacroWithArgument extends C2CpgSuite(withOssDataflow = true) {
  val cCode: String =
    """
      | #define MACRO(x) ((x) * (x))
      | #define MACRO_CONST 42
      | int main() {
      |   int a = MACRO_CONST;
      |   int w = MACRO(a); // int w = ((a) * (a));
      |   int e = MACRO(w); // int e = ((w) * (w));
      |   printf("MACRO!\n");
      |   return 0;
      | }
      |""".stripMargin
  val cCpg: CDefaultTestCpg = code(cCode)
  val cTraversal: Iterator[Method] = cCpg.graph._nodes(25).asInstanceOf[Iterator[nodes.Method]]
  val cAstDotString: Iterator[String] = DotAstGenerator.dotAst(cTraversal, extendedView=true, withColoring=true)
  println("Standard Joern C AST:")
  println(cAstDotString.mkString)


  val (superCAstDotString: String, superCJoernAstDotString: String) = generateVASTDot(cCode)
  println("\n\n\nSuperC (V)AST (original data structure):")
  println(superCAstDotString)

  println("\n\n\nSuperC (V)AST (translated to JOERN VAST data structure):")
  println(superCJoernAstDotString)
}

class testForLoop extends C2CpgSuite(withOssDataflow = true) {
  val cCode: String =
    """
      | int main() {
      |   for (int i = 0; i < 10; i++) {
      |     printf(i);
      |   }
      |   return 0;
      | }
      |""".stripMargin
  val cCpg: CDefaultTestCpg = code(cCode)
  val cTraversal: Iterator[Method] = cCpg.graph._nodes(25).asInstanceOf[Iterator[nodes.Method]]
  val cAstDotString: Iterator[String] = DotAstGenerator.dotAst(cTraversal, extendedView=true, withColoring=true)
  println("Standard Joern C AST:")
  println(cAstDotString.mkString)


  val (superCAstDotString: String, superCJoernAstDotString: String) = generateVASTDot(cCode)
  println("\n\n\nSuperC (V)AST (original data structure):")
  println(superCAstDotString)

  println("\n\n\nSuperC (V)AST (translated to JOERN VAST data structure):")
  println(superCJoernAstDotString)
}

class testForLoopWithContinue extends C2CpgSuite(withOssDataflow = true) {
  val cCode: String =
    """
      | int main() {
      |   for (int i = 0; i < 10; i++) {
      |     printf(i);
      |     if (i == 1) continue;
      |   }
      |   return 0;
      | }
      |""".stripMargin
  val cCpg: CDefaultTestCpg = code(cCode)
  val cTraversal: Iterator[Method] = cCpg.graph._nodes(25).asInstanceOf[Iterator[nodes.Method]]
  val cAstDotString: Iterator[String] = DotAstGenerator.dotAst(cTraversal, extendedView=true, withColoring=true)
  println("Standard Joern C AST:")
  println(cAstDotString.mkString)


  val (superCAstDotString: String, superCJoernAstDotString: String) = generateVASTDot(cCode)
  println("\n\n\nSuperC (V)AST (original data structure):")
  println(superCAstDotString)

  println("\n\n\nSuperC (V)AST (translated to JOERN VAST data structure):")
  println(superCJoernAstDotString)
}

class testForLoopWithBreak extends C2CpgSuite(withOssDataflow = true) {
  val cCode: String =
    """
      | int main() {
      |   for (int i = 0; i < 10; i++) {
      |     printf(i);
      |     if (i == 1) break;
      |   }
      |   return 0;
      | }
      |""".stripMargin
  val cCpg: CDefaultTestCpg = code(cCode)
  val cTraversal: Iterator[Method] = cCpg.graph._nodes(25).asInstanceOf[Iterator[nodes.Method]]
  val cAstDotString: Iterator[String] = DotAstGenerator.dotAst(cTraversal, extendedView=true, withColoring=true)
  println("Standard Joern C AST:")
  println(cAstDotString.mkString)


  val (superCAstDotString: String, superCJoernAstDotString: String) = generateVASTDot(cCode)
  println("\n\n\nSuperC (V)AST (original data structure):")
  println(superCAstDotString)

  println("\n\n\nSuperC (V)AST (translated to JOERN VAST data structure):")
  println(superCJoernAstDotString)
}

class testWhileLoopWithBreak extends C2CpgSuite(withOssDataflow = true) {
  val cCode: String =
    """
      | int main() {
      |   int i = 0;
      |   while (i < 10) {
      |     printf(i);
      |     if (i == 1) break;
      |     i++;
      |   }
      |   return 0;
      | }
      |""".stripMargin
  val cCpg: CDefaultTestCpg = code(cCode)
  val cTraversal: Iterator[Method] = cCpg.graph._nodes(25).asInstanceOf[Iterator[nodes.Method]]
  val cAstDotString: Iterator[String] = DotAstGenerator.dotAst(cTraversal, extendedView=true, withColoring=true)
  println("Standard Joern C AST:")
  println(cAstDotString.mkString)


  val (superCAstDotString: String, superCJoernAstDotString: String) = generateVASTDot(cCode)
  println("\n\n\nSuperC (V)AST (original data structure):")
  println(superCAstDotString)

  println("\n\n\nSuperC (V)AST (translated to JOERN VAST data structure):")
  println(superCJoernAstDotString)
}

class testDoWhileLoopWithBreak extends C2CpgSuite(withOssDataflow = true) {
  val cCode: String =
    """
      | int main() {
      |   int i = 0;
      |   do {
      |     printf(i);
      |     if (i == 1) break;
      |     i++;
      |   } while (i < 10);
      |   return 0;
      | }
      |""".stripMargin
  val cCpg: CDefaultTestCpg = code(cCode)
  val cTraversal: Iterator[Method] = cCpg.graph._nodes(25).asInstanceOf[Iterator[nodes.Method]]
  val cAstDotString: Iterator[String] = DotAstGenerator.dotAst(cTraversal, extendedView=true, withColoring=true)
  println("Standard Joern C AST:")
  println(cAstDotString.mkString)


  val (superCAstDotString: String, superCJoernAstDotString: String) = generateVASTDot(cCode)
  println("\n\n\nSuperC (V)AST (original data structure):")
  println(superCAstDotString)

  println("\n\n\nSuperC (V)AST (translated to JOERN VAST data structure):")
  println(superCJoernAstDotString)
}


class testFunctionCalls extends C2CpgSuite(withOssDataflow = true) {
  val cCode: String =
    """
      | int main() {
      |   printf("a");
      |   printf("a", "b");
      |   printf("a", "b", "c");
      |   return 0;
      | }
      |""".stripMargin
  val cCpg: CDefaultTestCpg = code(cCode)
  val cTraversal: Iterator[Method] = cCpg.graph._nodes(25).asInstanceOf[Iterator[nodes.Method]]
  val cAstDotString: Iterator[String] = DotAstGenerator.dotAst(cTraversal, extendedView=true, withColoring=true)
  println("Standard Joern C AST:")
  println(cAstDotString.mkString)


  val (superCAstDotString: String, superCJoernAstDotString: String) = generateVASTDot(cCode)
  println("\n\n\nSuperC (V)AST (original data structure):")
  println(superCAstDotString)

  println("\n\n\nSuperC (V)AST (translated to JOERN VAST data structure):")
  println(superCJoernAstDotString)
}


class testEnnumDec extends C2CpgSuite(withOssDataflow = true) {
  val cCode: String =
    """
      | enum Woche {MON, DIE, MIT, DON, FRE, SAM, SON};
      | int main() {
      |
      |   return 0;
      | }
      |""".stripMargin
  val cCpg: CDefaultTestCpg = code(cCode)
  val cTraversal: Iterator[Method] = cCpg.graph._nodes(25).asInstanceOf[Iterator[nodes.Method]]
  val cAstDotString: Iterator[String] = DotAstGenerator.dotAst(cTraversal, extendedView=true, withColoring=true)
  println("Standard Joern C AST:")
  println(cAstDotString.mkString)


  val (superCAstDotString: String, superCJoernAstDotString: String) = generateVASTDot(cCode)
  println("\n\n\nSuperC (V)AST (original data structure):")
  println(superCAstDotString)

  println("\n\n\nSuperC (V)AST (translated to JOERN VAST data structure):")
  println(superCJoernAstDotString)
}


class testEnnumUse extends C2CpgSuite(withOssDataflow = true) {
  val cCode: String =
    """
      | enum Woche {MON, DIE, MIT, DON, FRE, SAM, SON};
      | int main() {
      |   Woche w = Woche.MON;
      |   return 0;
      | }
      |""".stripMargin
  val cCpg: CDefaultTestCpg = code(cCode)
  val cTraversal: Iterator[Method] = cCpg.graph._nodes(25).asInstanceOf[Iterator[nodes.Method]]
  val cAstDotString: Iterator[String] = DotAstGenerator.dotAst(cTraversal, extendedView=true, withColoring=true)
  println("Standard Joern C AST:")
  println(cAstDotString.mkString)


  val (superCAstDotString: String, superCJoernAstDotString: String) = generateVASTDot(cCode)
  println("\n\n\nSuperC (V)AST (original data structure):")
  println(superCAstDotString)

  println("\n\n\nSuperC (V)AST (translated to JOERN VAST data structure):")
  println(superCJoernAstDotString)
}


class testMulitFunctionDec extends C2CpgSuite(withOssDataflow = true) {
  val cCode: String =
    """
      | int foo(int a) {
      |   if (a == 0) {
      |     start_punkt:
      |     a = 3 + a;
      |   } else {
      |     a = 2 * a;
      |   }
      |   for (int i = 0; i < a; i++) {
      |     print(i);
      |   }
      |   while (a > 0) {
      |     a -= 1;
      |   }
      |   do {
      |     a += 2;
      |   } while (a < 20);
      |   int c = 42;
      |   int b = 1;
      |   switch(c) {
      |    case 2:
      |      b = b + a;
      |      break;
      |    case 3:
      |      b = a;
      |      break;
      |    default:
      |      b = 3 * b;
      |    }
      |   return a;
      | }
      | int bar(int a, int b) {
      |   int c = bar(a);
      |   return a + b;
      | }
      | int main() {
      | int z = 2;
      |   int a = bar(2, z * z);
      |   goto start_punkt;
      |   return 0;
      | }
      |""".stripMargin
  val cCpg: CDefaultTestCpg = code(cCode)
  val cTraversal: Iterator[Method] = cCpg.graph._nodes(25).asInstanceOf[Iterator[nodes.Method]]
  val cAstDotString: Iterator[String] = DotAstGenerator.dotAst(cTraversal, extendedView=true, withColoring=true)
  println("Standard Joern C AST:")
  println(cAstDotString.mkString)


  val (superCAstDotString, superCJoernAstDotString) = generateVASTDot(cCode)
  println("\n\n\nSuperC (V)AST (original data structure):")
  println(superCAstDotString)

  println("\n\n\nSuperC (V)AST (translated to JOERN VAST data structure):")
  println(superCJoernAstDotString)
}




class testMulitFunctionDec2 extends C2CpgSuite(withOssDataflow = true) {
  val cCode: String =
    """
      | int foo(int a) {
      |   if (a == 0) {
      |     start_punkt:
      |     a = 3 + a;
      |   } else {
      |     a = 2 * a;
      |   }
      |   return a;
      | }
      | int main() {
      | int z = 2;
      |   int a = bar(2, z * z);
      |   goto start_punkt;
      |   return 0;
      | }
      |""".stripMargin
  val cCpg: CDefaultTestCpg = code(cCode)
  val cTraversal: Iterator[Method] = cCpg.graph._nodes(25).asInstanceOf[Iterator[nodes.Method]]
  val cAstDotString: Iterator[String] = DotCpg14Generator.toDotCpg14(cTraversal, extendedView=true, withColoring=true,
    forceTreeStructure=true)
  println("Standard Joern C AST:")
  println(cAstDotString.mkString)


  val (superCAstDotString, superCJoernAstDotString) = generateVASTDot(cCode)
  println("\n\n\nSuperC (V)AST (original data structure):")
  println(superCAstDotString)

  println("\n\n\nSuperC (V)AST (translated to JOERN VAST data structure):")
  println(superCJoernAstDotString)
}



class testMulitFunctionDecAdvanst extends C2CpgSuite(withOssDataflow = true) {
  val cCode: String =
    """
      | #ifdef MACRO_1
      | int
      | #else
      | long
      | #endif
      | #ifdef MACRO_2
      | foo
      | #else
      | foot
      | #endif
      | (
      | #ifdef MACRO_3
      | int
      | #else
      | long
      | #endif
      | #ifdef MACRO_4
      | a
      | #else
      | z
      | #endif
      | #ifdef MACRO_5
      | , int b, int c
      | #else
      | , int b, int c, int d
      | #endif
      | ) {
      |   start_punkt:
      |   return a;
      | }
      | int bar(int a, int b) {
      |   int c = bar(a);
      |   return a + b;
      | }
      | int main() {
      |   int z = 0;
      |   int a =
      |   #ifdef MACRo_CALL_1
      |   bar
      |   #else
      |   foo
      |   #endif
      |   (
      |   #ifdef MACRo_CALL_2
      |   2
      |   #else
      |   z
      |   #endif
      |   #ifdef MACRo_CALL_3
      |   , z * z
      |   #else
      |   , z * 3, z
      |   #endif
      |   );
      |   goto start_punkt;
      |   return 0;
      | }
      |""".stripMargin
  val cCpg: TestCpg = code(cCode)
  val cTraversal: Iterator[Method] = cCpg.graph._nodes(25).asInstanceOf[Iterator[nodes.Method]]
  val cAstDotString: Iterator[String] = DotAstGenerator.dotAst(cTraversal, extendedView=true, withColoring=true)
  println("Standard Joern C AST:")
  println(cAstDotString.mkString)


  val (superCAstDotString, superCJoernAstDotString) = generateVASTDot(cCode)
  println("\n\n\nSuperC (V)AST (original data structure):")
  println(superCAstDotString)

  println("\n\n\nSuperC (V)AST (translated to JOERN VAST data structure):")
  println(superCJoernAstDotString)
}










class testDecVariable extends C2CpgSuite(withOssDataflow = true) {
  val cCode: String =
    """
      | int a = 2;
      | char c = 'a';
      | long l;
      | long x = y =5;
      | int m = 9, n = 10;
      |
      |""".stripMargin
  val cCpg: TestCpg = code(cCode)
  val cTraversal: Iterator[Method] = cCpg.graph._nodes(25).asInstanceOf[Iterator[nodes.Method]]
  val cAstDotString: Iterator[String] = DotAstGenerator.dotAst(cTraversal, extendedView=true, withColoring=true)
  println("Standard Joern C AST:")
  println(cAstDotString.mkString)


  val (superCAstDotString, superCJoernAstDotString) = generateVASTDot(cCode)
  println("\n\n\nSuperC (V)AST (original data structure):")
  println(superCAstDotString)

  println("\n\n\nSuperC (V)AST (translated to JOERN VAST data structure):")
  println(superCJoernAstDotString)
}

class testDecVariableSimple extends C2CpgSuite(withOssDataflow = true) {
  val cFilename: String = "test_c_file.c"
  val cCode: String =
    """
      | int a = 2;
      | char c = 'a';
      | long l;
      | int w() {
      |   return 1;
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

class testDecVariableExtended extends C2CpgSuite(withOssDataflow = true) {
  val cFilename: String = "test_c_file.c"
  val cCode: String =
    """
      | int a = 2;
      | char c = 'a';
      | int foo(long a, long b) {
      |   return a;
      | }
      | long l;
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
