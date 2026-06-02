package io.shiftleft.semanticcpg.dotgenerator

import io.circe.parser.decode
import io.shiftleft.codepropertygraph.generated.Properties
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.utils.MemberAccess
import org.apache.commons.lang3.StringUtils
import org.apache.commons.text.StringEscapeUtils

import scala.collection.immutable.HashMap
import scala.collection.mutable
import scala.language.postfixOps

object DotSerializer {

  private val DefaultCharLimit: Int = 50
  // maximum length of code fields in number of characters
  private lazy val maxCharLimit: Int =
    sys.env.get("JOERN_MAX_DOT_CODE_LENGTH").flatMap(_.toIntOption).getOrElse(DefaultCharLimit)

  case class Graph(
                    vertices: List[StoredNode],
                    edges: List[Edge],
                    subgraph: Map[String, Seq[StoredNode]] = HashMap.empty[String, Seq[StoredNode]]
                  ) {

    def ++(other: Graph): Graph = {
      Graph((this.vertices ++ other.vertices).distinct, (this.edges ++ other.edges).distinct)
    }

  }

  case class Edge(
                   src: StoredNode,
                   dst: StoredNode,
                   srcVisible: Boolean = true,
                   label: String = "",
                   edgeType: String = ""
                 )

  object Edge {
    def apply(src: StoredNode, dst: StoredNode, srcVisible: Boolean = true, label: String = "", edgeType: String = ""): Edge = {
      val presenceConditionLabel = computeLabel(src, dst, edgeType)
      val newLabel = if (label.nonEmpty && presenceConditionLabel.nonEmpty) {
        label + "\n" + presenceConditionLabel
      }
      else {
        label + presenceConditionLabel
      }

      new Edge(src, dst, srcVisible, newLabel, edgeType)

    }

    private def computeLabel(src: StoredNode, dst: StoredNode, edgeType: String): String = {
      src.propertyOption[String]("PRESENCE_CONDITION") match {
        case Some(presenceConditionMapSerialized) =>
          val presenceConditionMap = decode[Map[String, String]](presenceConditionMapSerialized).getOrElse(Map.empty)

          val edgeId = edgeType match {
            case "AST" => "AST" + dst.property[Integer]("ORDER").toString
            case "CFG" => {
              val x = "CFG" + dst.id().toString
              if(presenceConditionMap.keys.contains(x)){
                x
              }
              else{
                // We do this because during VCFG generation we do not know what the fringe node is
                "UNKNOWN"
              }
            }
            case "DDG" | "CDG" => "PDG" + dst.id().toString
            case _ => ""
          }
          presenceConditionMap.getOrElse(edgeId, "")
        case None => ""
      }
    }
  }


  def dotGraph(root: Option[AstNode] = None, graph: Graph,
               withEdgeTypes: Boolean = false, extended_view: Boolean = false): String = {
    val sb = root match {
      case Some(r) => namedGraphBegin(r)
      case None => defaultGraphBegin()
    }

    sb.append(s"""node [shape="rect"];  \n""")
    val nodeStrings = graph.vertices.map(node => nodeToDot(node, extended_view))
    val edgeStrings = graph.edges.map(e => edgeToDot(e, withEdgeTypes))
    val subgraphStrings = graph.subgraph.zipWithIndex.map { case ((subgraph, nodes), idx) =>
      nodesToSubGraphs(subgraph, nodes, idx)
    }
    sb.append((nodeStrings ++ edgeStrings ++ subgraphStrings).mkString("\n"))
    graphEnd(sb)
  }

  private def namedGraphBegin(root: AstNode): mutable.StringBuilder = {
    val sb = new mutable.StringBuilder
    val name = StringEscapeUtils.escapeHtml4(root match {
      case method: Method => method.name
      case _ => ""
    })
    sb.append(s"""digraph "$name" {  \n""")
  }

  private def defaultGraphBegin(): mutable.StringBuilder = {
    val sb = new mutable.StringBuilder
    val name = "CPG"
    sb.append(s"""digraph "$name" {  \n""")
  }

  private def limit(str: String): String = StringUtils.abbreviate(str, maxCharLimit)

  private def stringRepr(vertex: StoredNode): String = {
    val lineOpt = vertex.property(Properties.LineNumber).map(_.toString)
    val attrList = (vertex match {
      case call: Call => List(call.name, limit(call.code))
      case ctrl: ControlStructure => List(ctrl.label, ctrl.controlStructureType, limit(ctrl.code))
      case expr: Expression => List(expr.label, limit(expr.code), limit(toCfgNode(expr).code))
      case method: Method => List(method.label, method.name)
      case ret: MethodReturn => List(ret.label, ret.typeFullName)
      case param: MethodParameterIn => List("PARAM", limit(param.code))
      case local: Local => List(local.label, s"${limit(local.code)}: ${local.typeFullName}")
      case target: JumpTarget => List(target.label, target.name)
      case modifier: Modifier => List(modifier.label, modifier.modifierType)
      case annoAssign: AnnotationParameterAssign => List(annoAssign.label, limit(annoAssign.code))
      case annoParam: AnnotationParameter => List(annoParam.label, limit(annoParam.code))
      case typ: Type => List(typ.label, typ.name)
      case typeDecl: TypeDecl => List(typeDecl.label, typeDecl.name)
      case member: Member => List(member.label, member.name)
      case _ => List.empty
    }).map(l => StringEscapeUtils.escapeHtml4(StringUtils.normalizeSpace(l)))

    (lineOpt match {
      case Some(line) => s"${attrList.head}, $line" :: attrList.tail
      case None => attrList
    }).distinct.mkString("<BR/>")
  }

  private def toCfgNode(node: StoredNode): CfgNode = {
    node match {
      case node: Identifier => node.parentExpression.get
      case node: MethodRef => node.parentExpression.get
      case node: Literal => node.parentExpression.get
      case node: Call if MemberAccess.isGenericMemberAccessName(node.name) => node.parentExpression.get
      case node: MethodParameterOut => node.method.methodReturn
      case node: MethodParameterIn => node.method
      case node: CallRepr => node
      case node: MethodReturn => node
      case node: Expression => node
    }
  }

  private def nodeToDot(node: StoredNode, extended_view: Boolean = false): String = {
    if (extended_view) {
      val className: String = node.getClass.toString.replace("<", "&lt;").replace(">", "&gt;").replace("&", "\t&amp;")
      var debugParam = "Debug parameters:"
      for (entry <- node._debugChildren()) {
        val newP = entry match {
          case e if e.toString.startsWith("AST") => ""
          case e if e.toString.startsWith("CFG") => ""
          case e if e.toString.startsWith("CPG") => ""
          case e if e.toString.startsWith("CDG") => ""
          case e if e.toString.startsWith("REF") => ""
          case e if e.toString.startsWith("CALL") => ""
          case e if e.toString.startsWith("PARAMETER_LINK") => ""
          case e if e.toString.startsWith("REACHING_DEF") => ""
          case e if e.toString.startsWith("CONTAINS") => ""
          case e if e.toString.startsWith("EVAL_TYPE") => ""
          case e if e.toString.startsWith("DOMINATE") => ""
          case e if e.toString.startsWith("POST_DOMINATE") => ""
          case e if e.toString.startsWith("ARGUMENT") => ""
          case e =>
            val debugParameters: String  = e.toString
              .replace("<", "&lt;")
              .replace(">", "&gt;")
              .replace("&", "\t&amp;")
              .replace("\n", "\\n")
            s"<br/> - \"$debugParameters\""
        }
        debugParam = debugParam + newP
      }

      val style: String = node match {
        case n: Method => " color=\"#8cb63c\" style=filled fillcolor=\"#e8f5d8\""
        case n: MethodParameterIn => " color=\"#8cb63c\" style=filled fillcolor=\"#e8f5d8\""
        case n: MethodReturn => " color=\"#8cb63c\" style=filled fillcolor=\"#e8f5d8\""
        case n: Call =>
          if (n._debugChildren().exists(e => e.toString.startsWith("NAME=<"))) {
            ""
          } else {
            " color=\"#009682\" style=filled fillcolor=\"#d9f1e6\""
          }
        case n: Return => " color=\"#23a1e0\" style=filled fillcolor=\"#dcf2fb\""
        case n: JumpTarget =>
          if (n._debugChildren().exists(e => e.toString.startsWith("PARSER_TYPE_NAME=CASTLabelStatement"))) {
            " color=\"#a97e23\" style=filled fillcolor=\"#f0e6d2\""
          } else {
            " color=\"#4664AA\" style=filled fillcolor=\"#e0e3f4\""
          }
        case n: Literal => " color=\"#df9b1b\" style=filled fillcolor=\"#fdecd2\""
        case n: Identifier => " color=\"#df9b1b\" style=filled fillcolor=\"#fdecd2\""
        case n: ControlStructure =>
          if (n._debugChildren().exists(e => e.toString.startsWith("CONTROL_STRUCTURE_TYPE=GOTO"))) {
            " color=\"#a97e23\" style=filled fillcolor=\"#f0e6d2\""
          } else if (n._debugChildren().exists(e => e.toString.startsWith("CONTROL_STRUCTURE_TYPE=BREAK"))) {
            " color=\"#23a1e0\" style=filled fillcolor=\"#dcf2fb\""
          } else if (n._debugChildren().exists(e => e.toString.startsWith("CONTROL_STRUCTURE_TYPE=CONTINUE"))) {
            " color=\"#23a1e0\" style=filled fillcolor=\"#dcf2fb\""
          } else if (n._debugChildren().exists(e => e.toString.startsWith("CONTROL_STRUCTURE_TYPE=CHOICE"))) {
            " color=\"#a3107c\" style=filled fillcolor=\"#f2d6ed\""
          } else {
            " color=\"#4664AA\" style=filled fillcolor=\"#e0e3f4\""
          }
        case _ => ""
      }

      var nodeInformation: String = stringRepr(node)
      if (nodeInformation.isEmpty) nodeInformation = " "
      s""""${node.id}" [label = <${className}<br/><b>${nodeInformation}</b><br/>${debugParam}> ${style}]""".stripMargin
    } else {
      s""""${node.id}" [label = <${stringRepr(node)}> ]""".stripMargin
    }
  }

  private def edgeToDot(edge: Edge, withEdgeTypes: Boolean): String = {
    val edgeLabel = if (withEdgeTypes) {
      edge.edgeType + ": " + StringEscapeUtils.escapeHtml4(edge.label)
    } else {
      StringEscapeUtils.escapeHtml4(edge.label)
    }
    val labelStr = Some(s""" [ label = "$edgeLabel"] """).filter(_ => edgeLabel != "").getOrElse("")
    s"""  "${edge.src.id}" -> "${edge.dst.id}" """ + labelStr
  }

  private def nodesToSubGraphs(subgraph: String, children: Seq[StoredNode], idx: Int): String = {
    val escapedName = StringEscapeUtils.escapeHtml4(subgraph)
    val childString = children.map { c => s"    \"${c.id()}\";" }.mkString("\n")
    s"""  subgraph cluster_$idx {
       |$childString
       |    label = "$escapedName";
       |  }
       |""".stripMargin
  }

  private def graphEnd(sb: mutable.StringBuilder): String = {
    sb.append("\n}\n")
    sb.toString
  }

}
