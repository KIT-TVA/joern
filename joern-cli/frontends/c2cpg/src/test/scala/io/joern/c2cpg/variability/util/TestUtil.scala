package io.joern.c2cpg.variability.util

import io.joern.c2cpg.astcreation.{CGlobal, VAstCreatorNew}
import io.joern.c2cpg.parser.FileDefaults
import io.joern.c2cpg.passes.variability.PdgPresenceConditionAnnotationPass
import io.joern.c2cpg.testfixtures.{AstC2CpgSuite, C2CpgSuite, CDefaultTestCpg}
import io.joern.dataflowengineoss.DefaultSemantics
import io.joern.dataflowengineoss.dotgenerator.{DotCpg14Generator, DotDdgGenerator, DotPdgGenerator}
import io.joern.dataflowengineoss.passes.reachingdef.ReachingDefPass
import io.joern.dataflowengineoss.semanticsloader.Semantics
import io.joern.x2cpg.X2Cpg
import io.joern.x2cpg.X2Cpg.newEmptyCpg
import io.joern.x2cpg.passes.frontend.MetaDataPass
import io.joern.x2cpg.testfixtures.Code2CpgFixture
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{Method, StoredNode}
import io.shiftleft.codepropertygraph.generated.{DiffGraphBuilder, Languages, nodes}
import io.shiftleft.semanticcpg.dotgenerator.{DotAstGenerator, DotCfgGenerator}
import superc.SuperC
import xtc.tree.{GNode, Location, Node}
import superc.core.{PresenceConditionManager, Syntax}
import superc.core.PresenceConditionManager.PresenceCondition

import scala.collection.mutable.Set
import scala.jdk.CollectionConverters.*
import java.io.{File, StringReader}


object TestUtil {

  private val JOERN_METHOD_NODE_KIND_ID: Int = 25
  private val GLOBAL_DOT_GRAPH_IDENTIFIER: String = "&lt;global&gt;"
  private val definedDotGraphNodeIDs: Set[Int] = Set.empty[Int]

  def generateVASTDot(cCode: String): (String, String) = 
    generateVASTDot(cCode, None, false)

  def generateVASTDot(cCode: String, cFileName: String): (String, String) = 
    generateVASTDot(cCode, Option(cFileName), false)

  def generateVASTDot(cCode: String, onlyGlobalGraph: Boolean): (String, String) = 
    generateVASTDot(cCode, None, onlyGlobalGraph)

  def generateVASTDot(cCode: String, cFileName: String, onlyGlobalGraph: Boolean): (String, String) = 
    generateVASTDot(cCode, Option(cFileName), onlyGlobalGraph)

  def generateVASTDot(cCode: String, cFileName: Option[String], onlyGlobalGraph: Boolean): (String, String) = {
    val (superCDotGraph, vcpg) = generateVCPG(cCode, cFileName = cFileName, onlyVAST = true)
    val astDotGraphs: Iterator[String] = DotAstGenerator.dotAst(vcpg, extended_view = true)
    if (onlyGlobalGraph) {
      (superCDotGraph,
        astDotGraphs.filter(dotGraph => dotGraph.startsWith(s"digraph \"$GLOBAL_DOT_GRAPH_IDENTIFIER\" {")).mkString)
    } else {
      (superCDotGraph, astDotGraphs.mkString)
    }
  }

  def generateVCFGDot(cCode: String): (String, String) = {
    val (superCDotGraph, vcpg) = generateVCPG(cCode, onlyVAST=false)
    (superCDotGraph, DotCfgGenerator.dotCfg(vcpg).mkString)
  }

  def generateVDDGDot(cCode: String): (String, String) = {
    val (superCDotGraph, vcpg) = generateVCPG(cCode, onlyVAST=false)
    (superCDotGraph, DotDdgGenerator.toDotDdg(vcpg).mkString)
  }

  def generateVPDGDot(cCode: String): (String, String) = {
    val (superCDotGraph, vcpg) = generateVCPG(cCode, onlyVAST=false)
    (superCDotGraph, DotPdgGenerator.toDotPdg(vcpg).mkString)
  }


  def generateVCPGDot(cCode: String): (String, String) = {
    val (superCDotGraph, vcpg) = generateVCPG(cCode, onlyVAST=false)
    (superCDotGraph, DotCpg14Generator.toDotCpg14(vcpg).mkString)
  }

  def generateVCPG(cCode: String, cFileName: Option[String] = None, onlyVAST: Boolean = true):  (String, Iterator[Method]) = {
    implicit val semantics: Semantics = DefaultSemantics()
    val stringReader = new StringReader(cCode)

    val dummyFile = if (cFileName.isDefined) new File(cFileName.get) else new File("test.c")
    val sup = new SuperC()
    sup.init()
    sup.prepare()

    // Creates the SuperC VAST and returns the SuperC VAST data structure.
    val superCParseResult: Node = sup.parse(stringReader, dummyFile)

    // Converts the SuperC VAST data structure into a dot graph.
    var superCDotGraph: String = superCGraphToDotGraph(superCParseResult, true)
    println(superCDotGraph)
    println("\n\n\n\n\n")
    superCDotGraph = superCGraphToDotGraph(superCParseResult, false)
    println(superCDotGraph)

    // General preparations for JOERN.
    val superCpg = newEmptyCpg(None)
    new MetaDataPass(superCpg, Languages.NEWC, "config.inputPath").createAndApply()

    val globalSuperC: CGlobal = new CGlobal()

    // Converts the SuperC VAST into a JOERN VAST.
    val vAstCreator = VAstCreatorNew("test.c", globalSuperC, superCParseResult)
    //val vAstCreator = VAstCreator("test.c", globalSuperC, superCParseResult)
    val diffGraph: DiffGraphBuilder = vAstCreator.createAst() // Baut JOERN-Graph (Diff Graph builder) | Haut SuperC-VAST in JOERN Datenbank | Kein JOERN-Datenstrukur bauen

    flatgraph.DiffGraphApplier.applyDiff(superCpg.graph, diffGraph)
    // For further graphs (except AST), CFG, ...
    X2Cpg.applyDefaultOverlays(superCpg) // Include CFG | Comment out if an errors occur

    // Not required for AST; needed only for the following graphs (CFG, ...).
    if (!onlyVAST) {
      new ReachingDefPass(superCpg).createAndApply()
      new PdgPresenceConditionAnnotationPass(superCpg).createAndApply()
    }

    // Gibt den SuperC-VAST als Dot-Graph zurück und
    (superCDotGraph, superCpg.graph._nodes(JOERN_METHOD_NODE_KIND_ID).asInstanceOf[Iterator[nodes.Method]])
  }

  /**
   * Converts the SuperC CPG into a dot graph.
   *
   * @param superCpg The SuperC CPG data structure.
   * @return Returns the SuperC CPG as dot graph.
   */
  def superCGraphToDotGraph(superCpg: Node, showObjectGraph: Boolean = false): String = {
    definedDotGraphNodeIDs.clear()
    val rootDotGraphNodeID: Int = if (showObjectGraph) System.identityHashCode(superCpg) else 0
    val (graph: String, _) = superCGraphToDotGraphHelper(superCpg, rootDotGraphNodeID, showObjectGraph)
    definedDotGraphNodeIDs.clear()
    s"digraph SupcerC_AST {\n  graph [rankdir=TB];\n  node [shape=box];\n  edge [color=gray];\n\n$graph}"
  }

  def processSuperCNode(node: Node, dotGraphNodeID: Int, showObjectGraph: Boolean): (String, Int) = {
    val location: Location = node.getLocation
    var propertyString: String = "<br/><i>file undefined</i>"
    if (location != null) {
      propertyString =
        s"<br/>file: \"<i>${location.file}</i>\", line: <i>${location.line}</i>, column: <i>${location.column}</i>"
    }
    val propertyNames = node.properties
    propertyString = propertyString + s"<br/>properties (number: ${propertyNames.size})"
    for (propertyName: String <- propertyNames.asScala) {
      propertyString = propertyString + s"<br/> - \"$propertyName: &lt;value is not shown&gt;"
      // propertyString = propertyString + s"<br/> - \"${propertyName}: \"<i>${node.getStringProperty(propertyName)}</i>\""
    }

    val style: String = node.getName match {
      case "SelectionStatement" => " color=\"#4664AA\" style=filled fillcolor=\"#e0e3f4\""
      case "LabeledStatement" => " color=\"#4664AA\" style=filled fillcolor=\"#e0e3f4\""
      case "IterationStatement" => " color=\"#4664AA\" style=filled fillcolor=\"#e0e3f4\""
      case "BreakStatement" => " color=\"#23a1e0\" style=filled fillcolor=\"#dcf2fb\""
      case "ContinueStatement" => " color=\"#23a1e0\" style=filled fillcolor=\"#dcf2fb\""
      case "ReturnStatement" => " color=\"#23a1e0\" style=filled fillcolor=\"#dcf2fb\""
      case "Conditional" => " color=\"#a3107c\" style=filled fillcolor=\"#f2d6ed\""
      case "FunctionDefinition" => " color=\"#8cb63c\" style=filled fillcolor=\"#e8f5d8\""
      case "FunctionPrototype" => " color=\"#8cb63c\" style=filled fillcolor=\"#e8f5d8\""
      case "FunctionDeclarator" => " color=\"#8cb63c\" style=filled fillcolor=\"#e8f5d8\""
      case "PostfixingFunctionDeclarator" => " color=\"#8cb63c\" style=filled fillcolor=\"#e8f5d8\""
      case "FunctionCall" =>  " color=\"#009682\" style=filled fillcolor=\"#d9f1e6\""
      case "GotoStatement" =>  " color=\"#a97e23\" style=filled fillcolor=\"#f0e6d2\""
      case name if name.startsWith("superc.core.Syntax$") => " color=\"#df9b1b\" style=filled fillcolor=\"#fdecd2\""
      case _ => ""
    }

    definedDotGraphNodeIDs.add(dotGraphNodeID)
    val objectAddress: String = java.lang.Integer.toHexString(System.identityHashCode(node))
    val nodeDefinition: String = s"  \"${dotGraphNodeID.toString}\" [label=<<i>0x$objectAddress</i><br/>" +
      s"${node.getClass.toString}<br/><b>${node.getName}</b>$propertyString>$style]\n"
    var subGraphs: String = ""
    var edges: String = ""
    var newDotGraphNodeID: Int = dotGraphNodeID
    for (index: Int <- 0 until node.size) {
      newDotGraphNodeID = if (showObjectGraph) System.identityHashCode(node.get(index)) else newDotGraphNodeID + 1

      val edge: String = s"  \"$dotGraphNodeID\" -> \"$newDotGraphNodeID\" [label=\"[$index]\"];\n"
      val (subGraph: String, nextDotGraphNodeID: Int) =
        superCGraphToDotGraphHelper(node.get(index), newDotGraphNodeID, showObjectGraph)
      newDotGraphNodeID = nextDotGraphNodeID
      if (subGraph.equals("<<DUPLICATE>>")) {
        edges = edges + edge
      } else if (!subGraph.equals("")) {
        subGraphs = subGraphs + subGraph
        edges = edges + edge
      }
    }
    (nodeDefinition + subGraphs + edges, newDotGraphNodeID)
  }

  def processSuperCPresenceCondition(node: PresenceCondition, dotGraphNodeID: Int,
                                     showObjectGraph: Boolean): (String, Int) = {

    val condition: String = node.toString.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")
    val conditionalExpression: String = s"Condition:<br/>\"<i>$condition</i>\""
    
    var allConfigString: String = "Configurations:"
    val allConfigs = node.getAllConfigs.asScala
    for (config: String <- allConfigs) {
      val escaped_config: String = config.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")
      allConfigString = allConfigString + s"<br/> - \"<i>$escaped_config</i>\""
    }

    definedDotGraphNodeIDs.add(dotGraphNodeID)
    val bdd = node.getBDD
    val bddToString: String = bdd.toString.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")
    val bddToStringWithDomains: String = bdd.toStringWithDomains.replace("<", "&lt;").replace(">", "&gt;")
    val className: String = node.getClass.toString.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")
    val objectAddress: String = java.lang.Integer.toHexString(System.identityHashCode(node))
    val conditionalNode: String = s"  \"$dotGraphNodeID\" [label=<<i>0x$objectAddress</i><br/>$className<br/>" +
      s"$conditionalExpression<br/>$allConfigString<br/><br/>BDD.toString: \"<i>$bddToString</i>\"<br/>" +
      s"BDD.toStringWithDomains: \"<i>$bddToStringWithDomains</i>\"<br/><i>maybe an incomplete subtree</i>> " +
      "color=\"#a22223\" style=filled fillcolor=\"#f2d5cb\"];\n"

    val subtreeNode = node.tree()
    if (subtreeNode == null) return (conditionalNode, dotGraphNodeID)

    val nextDotGraphNodeID: Int = if (showObjectGraph) System.identityHashCode(node.tree()) else dotGraphNodeID + 1
    val edge = s"  \"$dotGraphNodeID\" -> \"$nextDotGraphNodeID\"\n"
    val (subtree: String, newDotGraphNodeID: Int) =
      superCGraphToDotGraphHelper(node.tree(), nextDotGraphNodeID, showObjectGraph)
    (conditionalNode + subtree + edge, newDotGraphNodeID)
  }

  def superCGraphToDotGraphHelper(nodeD: Any, dotGraphNodeID: Int, showObjectGraph: Boolean): (String, Int) = {
    require(nodeD != null, "It seems that the C code parsed by SuperC contains a syntax error.")
    nodeD match {
      case node if definedDotGraphNodeIDs.contains(dotGraphNodeID) => ("<<DUPLICATE>>", dotGraphNodeID)
      case node: GNode  => processSuperCNode(node, dotGraphNodeID, showObjectGraph)
      case node: Syntax => processSuperCNode(node, dotGraphNodeID, showObjectGraph)
      case node: PresenceCondition => processSuperCPresenceCondition(node, dotGraphNodeID, showObjectGraph)

      case node if node.getClass.toString == "class java.lang.String" =>
        definedDotGraphNodeIDs.add(dotGraphNodeID)
        val className: String = node.toString.replace("<", "&lt;").replace(">", "&gt;")
        val objectAddress: String = java.lang.Integer.toHexString(System.identityHashCode(node))
        (s"  \"$dotGraphNodeID\" [label=<<i>0x$objectAddress</i><br/>${node.getClass.toString}<br/>content: \"<i>" +
          s"$className</i>\"> color=\"#fce500\" style=filled fillcolor=\"#fefbd8\"];\n", dotGraphNodeID)
      case node =>
        definedDotGraphNodeIDs.add(dotGraphNodeID)
        val className: String = node.toString.replace("<", "&lt;").replace(">", "&gt;")
        val objectAddress: String = java.lang.Integer.toHexString(System.identityHashCode(node))
        (s"  \"$dotGraphNodeID\" [label=<<i>0x$objectAddress</i><br/>$className<br/><i>maybe an incomplete " +
          "subtree</i>> color=\"#a22223\" style=filled fillcolor=\"#f2d5cb\"];\n", dotGraphNodeID)
    }
  }
}
