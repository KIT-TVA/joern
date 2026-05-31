package io.joern.c2cpg.variability.util

import io.joern.c2cpg.astcreation.{CGlobal, VAstCreator}
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

import scala.jdk.CollectionConverters.*
import java.io.{File, StringReader}


object TestUtil {

  val JOERN_METHOD_NODE_KIND_ID: Int = 25

  def generateVASTDot(cCode: String): (String, String) = {
    val (superCDotGraph, vcpg) = generateVCPG(cCode, cFileName=None, onlyVAST=true)
    (superCDotGraph, DotAstGenerator.dotAst(vcpg, extended_view=true).mkString)
  }

  def generateVASTDot(cCode: String, cFileName: String): (String, String) = {
    val (superCDotGraph, vcpg) = generateVCPG(cCode, cFileName=Option(cFileName), onlyVAST=true)
    (superCDotGraph, DotAstGenerator.dotAst(vcpg, extended_view = true).mkString)
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
    val superCDotGraph: String = superCGraphToDotGraph(superCParseResult)

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
  def superCGraphToDotGraph(superCpg: Node): String = {
    val (graph, _) = superCGraphToDotGraphHelper(superCpg, 0)
    s"digraph SupcerC_AST {\n  graph [rankdir=TB];\n  node [shape=box];\n  edge [color=gray];\n\n${graph}}"
  }

  def processSuperCNode(node: Node, dotGraphNodeID: Int): (String, Int) = {
    val location: Location = node.getLocation
    var propertyString: String = "<br/><i>file undefined</i>"
    if (location != null) {
      propertyString =
        s"<br/>file: \"<i>${location.file}</i>\", line: <i>${location.line}</i>, column: <i>${location.column}</i>"
    }
    val propertyNames = node.properties
    propertyString = propertyString + s"<br/>properties (number: ${propertyNames.size})"
    for (propertyName: String <- propertyNames.asScala) {
      propertyString = propertyString + s"<br/> - \"${propertyName}: &lt;value is not shown&gt;"
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

    val nodeDefinition: String = s"  \"${dotGraphNodeID.toString}\" [label=<${node.getClass.toString}<br/>" +
      s"<b>${node.getName}</b>${propertyString}>${style}]\n"
    var subGraphs: String = ""
    var edges: String = ""
    var newDotGraphNodeID: Int = dotGraphNodeID
    for (index: Int <- 0 until node.size) {
      newDotGraphNodeID = newDotGraphNodeID + 1

      val edge: String = s"  \"${dotGraphNodeID}\" -> \"${newDotGraphNodeID}\" [label=\"[${index}]\"];\n"
      val (subGraph: String, nextDotGraphNodeID: Int) = superCGraphToDotGraphHelper(node.get(index), newDotGraphNodeID)
      newDotGraphNodeID = nextDotGraphNodeID
      if (subGraph != "") {
        subGraphs = subGraphs + subGraph
        edges = edges + edge
      }
    }
    (nodeDefinition + subGraphs + edges, newDotGraphNodeID)
  }

  def processSuperCPresenceCondition(node: PresenceCondition, dotGraphNodeID: Int): (String, Int) = {

    var allConfigString: String = "Configurations:"
    val allConfigs = node.getAllConfigs.asScala
    for (config: String <- allConfigs) allConfigString = allConfigString + s"<br/> - \"<i>${config}</i>\""

    val bdd = node.getBDD
    val bddToString: String = bdd.toString.replace("<", "&lt;").replace(">", "&gt;")
    val bddToStringWithDomains: String = bdd.toStringWithDomains.replace("<", "&lt;").replace(">", "&gt;")
    val className: String = node.getClass.toString.replace("<", "&lt;").replace(">", "&gt;")
    val conditionalNode: String = s"  \"${dotGraphNodeID}\" [label=<${className}<br/>${allConfigString}<br/><br/>" +
      s"BDD.toString: \"<i>${bddToString}</i>\"<br/>BDD.toStringWithDomains: \"<i>${bddToStringWithDomains}</i>\"<br/>" +
      "<i>maybe an incomplete subtree</i>> color=\"#a22223\" style=filled fillcolor=\"#f2d5cb\"];\n"

    val subtreeNode = node.tree()
    if (subtreeNode == null) return (conditionalNode, dotGraphNodeID)

    val edge = "  \"" + dotGraphNodeID + "\" -> \"" + (dotGraphNodeID + 1) + "\"\n"
    val (subtree: String, newDotGraphNodeID: Int) = superCGraphToDotGraphHelper(node.tree(), dotGraphNodeID + 1)
    (conditionalNode + subtree + edge, newDotGraphNodeID)
  }

  def superCGraphToDotGraphHelper(nodeD: Any, dotGraphNodeID: Int): (String, Int) = {
    nodeD match {
      case node: GNode  => processSuperCNode(node, dotGraphNodeID)
      case node: Syntax => processSuperCNode(node, dotGraphNodeID)
      case node: PresenceCondition => processSuperCPresenceCondition(node, dotGraphNodeID)

      case node if node.getClass.toString == "class java.lang.String" =>
        val className: String = node.toString.replace("<", "&lt;").replace(">", "&gt;")
        (s"  \"${dotGraphNodeID}\" [label=<${node.getClass.toString}<br/>content: \"<i>${className}</i>\"> " +
          "color=\"#fce500\" style=filled fillcolor=\"#fefbd8\"];\n", dotGraphNodeID)
      case node =>
        val className: String = node.toString.replace("<", "&lt;").replace(">", "&gt;")
        (s"  \"${dotGraphNodeID}\" [label=<${className}<br/><i>maybe an incomplete subtree</i>> color=\"#a22223\" " +
          "style=filled fillcolor=\"#f2d5cb\"];\n", dotGraphNodeID)
    }
  }
}
