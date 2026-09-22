package io.joern.c2cpg.astcreation.converter

import io.joern.c2cpg.astcreation.VAstCreatorNew
import io.joern.x2cpg.Ast
import io.shiftleft.codepropertygraph.generated.ControlStructureTypes
import xtc.tree.{Location, Node}

/**
 * SuperC: `goto` + plain labels (Task 8, plain).
 *
 * Joern shape (same as standard c2cpg):
 *   CONTROL_STRUCTURE TYPE=GOTO  (code "goto label;")
 *   JUMP_TARGET for `label:` (name = label text)
 *
 * case/default labels are handled by [[VAstPatternConverterForSwitch]].
 *
 * SuperC names often look like:
 *   IdentifierOrTypedefName(superc.core.Syntax$Text["done"])
 */
class VAstPatternConverterForGoto(vAstCreator: VAstCreatorNew, converter: VAstConverter)
  extends VAstPatternConverter(
    vAstCreator,
    converter,
    List(
      "GotoStatement",
      "JumpStatement",
      "LabeledStatement"
    )
  ) {

  override def convert(superCVAst: Node, converterState: VAstConverterState): Option[Seq[Ast]] = {
    superCVAst.getName match {
      case "GotoStatement" =>
        convertGoto(superCVAst).map(Seq(_))
      case "JumpStatement" if keywordAt(superCVAst, 0) == "goto" =>
        convertGoto(superCVAst).map(Seq(_))
      case "LabeledStatement" if isPlainLabel(superCVAst) =>
        Option(convertPlainLabel(superCVAst, converterState))
      case _ => None
    }
  }

  private def convertGoto(gotoNode: Node): Option[Ast] = {
    val label = labelName(gotoNode)
    if (label.isEmpty) None
    else {
      val (line, column) = locationOf(gotoNode)
      val code           = s"goto $label;"
      val node =
        vAstCreator.controlStructureNodeHelper(gotoNode, ControlStructureTypes.GOTO, code, line, column)
      Option(vAstCreator.AstHelper(node))
    }
  }

  private def convertPlainLabel(labelNode: Node, converterState: VAstConverterState): Seq[Ast] = {
    val name           = plainLabelName(labelNode)
    val (line, column) = locationOf(labelNode)
    val code           = if (name.nonEmpty) s"$name:" else "label:"
    val jump =
      vAstCreator.AstHelper(vAstCreator.jumpTargetNodeHelper(labelNode, name, code, line, column))
    jump +: nestedStatementAsts(labelNode, converterState)
  }

  private def isPlainLabel(node: Node): Boolean = {
    val kw = keywordAt(node, 0)
    kw != "case" && kw != "default" && kw != "goto" && plainLabelName(node).nonEmpty
  }

  private def plainLabelName(node: Node): String = {
    val fromKeyword = keywordAt(node, 0)
    if (fromKeyword.matches("[A-Za-z_][A-Za-z0-9_]*") &&
      fromKeyword != "case" && fromKeyword != "default" && fromKeyword != "goto") {
      fromKeyword
    } else {
      safeNodeAt(node, 0).flatMap(extractName).orElse(extractNameFromAny(node.get(0))).getOrElse("")
    }
  }

  private def labelName(gotoNode: Node): String = {
    val offset = if (keywordAt(gotoNode, 0) == "goto") 1 else 0
    if (gotoNode.size() <= offset) ""
    else
      gotoNode.get(offset) match {
        case s: String if s.matches("[A-Za-z_][A-Za-z0-9_]*") => s
        case s: String if s != ";" && s != "goto" =>
          extractQuotedName(s).getOrElse("")
        case other =>
          safeNodeAt(gotoNode, offset)
            .flatMap(extractName)
            .orElse(extractNameFromAny(other))
            .getOrElse("")
      }
  }

  /** Unwrap PrimaryIdentifier / IdentifierOrTypedefName / Syntax$Text → "done". */
  private def extractName(node: Node): Option[String] = {
    val name = Option(node.getName).getOrElse("")
    if (name.contains("Syntax$Text") || name == "Text") {
      firstString(node).filter(_.matches("[A-Za-z_][A-Za-z0-9_]*"))
        .orElse(extractQuotedName(node.toString))
    } else if (
      name == "PrimaryIdentifier" ||
        name == "IdentifierOrTypedefName" ||
        name.contains("Identifier")
    ) {
      if (node.size() > 0) {
        node.get(0) match {
          case s: String if s.matches("[A-Za-z_][A-Za-z0-9_]*") => Some(s)
          case child: Node                                     => extractName(child)
          case _ =>
            try extractName(node.getNode(0))
            catch { case _: Exception => extractQuotedName(node.toString) }
        }
      } else extractQuotedName(node.toString)
    } else {
      firstString(node).filter(_.matches("[A-Za-z_][A-Za-z0-9_]*"))
        .orElse(extractQuotedName(node.toString))
    }
  }

  private def extractNameFromAny(value: Any): Option[String] = value match {
    case null                                => None
    case s: String if s.matches("[A-Za-z_][A-Za-z0-9_]*") => Some(s)
    case s: String                           => extractQuotedName(s)
    case n: Node                             => extractName(n)
    case other                               => extractQuotedName(other.toString)
  }

  /** From `...Syntax$Text["done"]...` or `Text("done")`. */
  private def extractQuotedName(text: String): Option[String] = {
    val patterns = Seq(
      """\["([A-Za-z_][A-Za-z0-9_]*)"\]""".r,
      """\('([A-Za-z_][A-Za-z0-9_]*)'\)""".r,
      """Text\["([A-Za-z_][A-Za-z0-9_]*)"\]""".r
    )
    patterns.view.flatMap(_.findFirstMatchIn(text).map(_.group(1))).headOption
  }

  private def firstString(node: Node): Option[String] =
    try Option(node.getString(0)).filter(_.nonEmpty)
    catch {
      case _: Exception =>
        if (node.size() <= 0) None
        else
          node.get(0) match {
            case s: String => Some(s)
            case _         => None
          }
    }

  private def nestedStatementAsts(labelNode: Node, converterState: VAstConverterState): Seq[Ast] = {
    val kids = getChildren(labelNode)
    // Skip the label name child; convert remaining statement children.
    kids.drop(1).flatMap { child =>
      val n = Option(child.getName).getOrElse("")
      if (n == ":" || n.isEmpty) Seq.empty
      else converter.convert(child, converterState)
    }
  }

  private def keywordAt(node: Node, index: Int): String =
    if (index >= node.size()) ""
    else
      node.get(index) match {
        case value: String => value
        case _             => ""
      }

  private def safeNodeAt(node: Node, index: Int): Option[Node] =
    if (index < 0 || index >= node.size()) None
    else
      node.get(index) match {
        case child: Node => Some(child)
        case _: String   => None
        case _ =>
          try Option(node.getNode(index))
          catch { case _: Exception => None }
      }

  private def getChildren(node: Node): Seq[Node] =
    (0 until node.size()).flatMap(i => safeNodeAt(node, i))

  private def locationOf(node: Node): (Option[Int], Option[Int]) = {
    val loc: Location = node.getLocation
    if (loc == null) (None, None) else (Option(loc.line), Option(loc.column))
  }
}
