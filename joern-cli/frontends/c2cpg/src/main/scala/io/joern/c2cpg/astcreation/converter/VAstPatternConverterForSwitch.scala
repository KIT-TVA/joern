package io.joern.c2cpg.astcreation.converter

import io.joern.c2cpg.astcreation.{Defines, VAstCreatorNew}
import io.joern.x2cpg.Ast
import io.shiftleft.codepropertygraph.generated.nodes.{NewIdentifier, NewLiteral}
import io.shiftleft.codepropertygraph.generated.ControlStructureTypes
import xtc.tree.{Location, Node}

/**
 * SuperC: `switch` / `case` / `default` (Task 13).
 *
 * Joern: SWITCH + condition; body with JUMP_TARGET (+ case expr) / default / stmts.
 * `#ifdef` → CHOICE via ConditionalHandler; Conditional("1", …) is unwrapped.
 */
class VAstPatternConverterForSwitch(vAstCreator: VAstCreatorNew, converter: VAstConverter)
  extends VAstPatternConverter(
    vAstCreator,
    converter,
    List(
      "SelectionStatement",
      "SwitchStatement",
      "LabeledStatement",
      "CaseStatement",
      "DefaultStatement",
      "CaseLabeledStatement",
      "DefaultLabeledStatement"
    )
  ) {

  override def convert(superCVAst: Node, converterState: VAstConverterState): Option[Seq[Ast]] = {
    superCVAst.getName match {
      case "SelectionStatement" if keywordAt(superCVAst, 0).contains("switch") =>
        convertSwitch(superCVAst, converterState).map(Seq(_))
      case "SwitchStatement" =>
        convertSwitch(superCVAst, converterState).map(Seq(_))
      case "CaseStatement" | "CaseLabeledStatement" =>
        Option(convertCase(superCVAst, converterState))
      case "DefaultStatement" | "DefaultLabeledStatement" =>
        Option(convertDefault(superCVAst, converterState))
      case "LabeledStatement" =>
        keywordAt(superCVAst, 0) match {
          case kw if kw.contains("case") && !kw.contains("cased") =>
            Option(convertCase(superCVAst, converterState))
          case kw if kw.contains("default") =>
            Option(convertDefault(superCVAst, converterState))
          case _ => None
        }
      case _ => None
    }
  }

  private def convertSwitch(switchNode: Node, converterState: VAstConverterState): Option[Ast] = {
    val offset = if (keywordAt(switchNode, 0).contains("switch")) 1 else 0
    if (switchNode.size() < offset + 2) None
    else {
      val conditionAst   = convertExpr(switchNode.getNode(offset), converterState)
      val bodyAst        = convertBody(switchNode.getNode(offset + 1), converterState)
      val (line, column) = locationOf(switchNode)
      val code           = s"switch (${astCode(conditionAst)})"
      val ctrl =
        vAstCreator.controlStructureNodeHelper(switchNode, ControlStructureTypes.SWITCH, code, line, column)
      Option(vAstCreator.controlStructureAst(ctrl, Option(conditionAst), Seq(bodyAst)))
    }
  }

  /** JUMP_TARGET("case") + LITERAL/IDENT for the case value + nested statement. */
  private def convertCase(caseNode: Node, converterState: VAstConverterState): Seq[Ast] = {
    val (line, column) = locationOf(caseNode)
    val value          = caseValue(caseNode)
    val exprAst        = value.map(v => leafAst(v, line, column))
    val code           = value.map(v => s"case $v:").getOrElse("case:")
    val jump =
      vAstCreator.AstHelper(vAstCreator.jumpTargetNodeHelper(caseNode, "case", code, line, column))
    Seq(jump) ++ exprAst.toSeq ++ nestedStmt(caseNode, converterState)
  }

  private def convertDefault(defaultNode: Node, converterState: VAstConverterState): Seq[Ast] = {
    val (line, column) = locationOf(defaultNode)
    val jump = vAstCreator.AstHelper(
      vAstCreator.jumpTargetNodeHelper(defaultNode, "default", "default:", line, column)
    )
    Seq(jump) ++ nestedStmt(defaultNode, converterState)
  }

  /** SuperC: LabeledStatement(Language("case"), Text("0"), stmt…) */
  private def caseValue(caseNode: Node): Option[String] = {
    val start = if (keywordAt(caseNode, 0).contains("case")) 1 else 0
    (start until caseNode.size()).view.flatMap(i => slotText(caseNode, i)).find(t => t != ":" && t != "case")
  }

  /** Read string from a child slot (String / Text / Conditional("1", Text)). */
  private def slotText(parent: Node, index: Int): Option[String] =
    parent.get(index) match {
      case s: String => Option(s).filter(_.nonEmpty)
      case n: Node =>
        val name = Option(n.getName).getOrElse("")
        if (name.contains("Statement") || name.contains("Language")) None
        else if (name == "Conditional") {
          val h = converter.getConditionalHandler
          if (h.isConditionalNode(n) && h.getFirstCondition(n) == "1")
            textOf(h.getFirstConditionalSubtree(n))
          else None
        } else textOf(n)
      case _ =>
        try textOf(parent.getNode(index))
        catch { case _: Exception => None }
    }

  private def nestedStmt(labelNode: Node, converterState: VAstConverterState): Seq[Ast] =
    getChildren(labelNode).lastOption match {
      case Some(stmt) if Option(stmt.getName).exists(n => n.contains("Statement") || n == "Conditional") =>
        convertChild(stmt, converterState)
      case _ => Seq.empty
    }

  /**
   * Always returns a BLOCK (never empty Ast), so SWITCH keeps an AST child.
   * SuperC often wraps the switch body as Conditional("1", CompoundStatement).
   */
  private def convertBody(bodyNode: Node, converterState: VAstConverterState): Ast = {
    val h = converter.getConditionalHandler
    if (h.isConditionalNode(bodyNode) && h.getFirstCondition(bodyNode) == "1") {
      convertBody(h.getFirstConditionalSubtree(bodyNode), converterState)
    } else if (h.isConditionalNode(bodyNode)) {
      wrapInBlock(bodyNode, h.handelConditional(bodyNode, converterState, (n, s) => convertChild(n, s)))
    } else if (bodyNode.getName == "CompoundStatement" && bodyNode.size() >= 2) {
      val stmtAsts =
        getChildren(bodyNode.getNode(1)).flatMap(c => convertChild(c, converterState)).filterNot(isDummy)
      wrapInBlock(bodyNode, stmtAsts)
    } else {
      wrapInBlock(bodyNode, convertChild(bodyNode, converterState).filterNot(isDummy))
    }
  }

  private def wrapInBlock(node: Node, stmts: Seq[Ast]): Ast = {
    val kept           = stmts.filter(_.root.isDefined)
    val (line, column) = locationOf(node)
    val code           = kept.map(astCode).filter(_.nonEmpty).mkString("\n")
    val block =
      vAstCreator.blockNodeHelper(node, if (code.nonEmpty) code else "<empty>", "<???>", line, column)
    vAstCreator.blockAstHelper(block, kept.toList)
  }

  /** Unwrap Conditional("1",…); real #ifdef → CHOICE. */
  private def convertChild(node: Node, converterState: VAstConverterState): Seq[Ast] = {
    val h = converter.getConditionalHandler
    if (h.isConditionalNode(node)) {
      if (h.getFirstCondition(node) == "1") convertChild(h.getFirstConditionalSubtree(node), converterState)
      else h.handelConditional(node, converterState, (n, s) => convertChild(n, s))
    } else if (node.getName == "CompoundStatement") {
      Seq(convertBody(node, converterState))
    } else {
      converter.convert(node, converterState)
    }
  }

  private def convertExpr(node: Node, converterState: VAstConverterState): Ast = {
    val h = converter.getConditionalHandler
    if (h.isConditionalNode(node) && h.getFirstCondition(node) == "1")
      convertExpr(h.getFirstConditionalSubtree(node), converterState)
    else {
      val converted = converter.convert(node, converterState)
      if (converted.nonEmpty && converted.head.root.isDefined && !isDummy(converted.head)) converted.head
      else {
        val (line, column) = locationOf(node)
        textOf(node).map(t => leafAst(t, line, column)).getOrElse {
          if (node.size() == 1 && node.get(0).isInstanceOf[Node]) convertExpr(node.getNode(0), converterState)
          else converted.headOption.getOrElse(vAstCreator.AstHelper())
        }
      }
    }
  }

  private def leafAst(text: String, line: Option[Int], column: Option[Int]): Ast =
    if (text.matches("[A-Za-z_][A-Za-z0-9_]*"))
      vAstCreator.AstHelper(
        NewIdentifier().name(text).code(text).typeFullName(Defines.Any).lineNumber(line).columnNumber(column)
      )
    else
      vAstCreator.AstHelper(NewLiteral().code(text).typeFullName("int").lineNumber(line).columnNumber(column))

  private def textOf(node: Node): Option[String] = {
    val direct = firstStringChild(node)
    if (direct.nonEmpty) Some(direct)
    else {
      val s = node.toString
      Seq("""\["([^"]+)"\]""".r, """\("([^"]+)"\)""".r).view
        .flatMap(_.findFirstMatchIn(s).map(_.group(1)))
        .headOption
    }
  }

  private def firstStringChild(node: Node): String = {
    var i = 0
    while (i < node.size()) {
      node.get(i) match {
        case value: String if value.nonEmpty => return value
        case value: Number                   => return value.toString
        case child: Node =>
          val nested = firstStringChild(child)
          if (nested.nonEmpty) return nested
        case _ =>
          try {
            val s = node.getString(i)
            if (s != null && s.nonEmpty) return s
          } catch { case _: Exception => }
      }
      i += 1
    }
    ""
  }

  private def keywordAt(node: Node, index: Int): String =
    if (index >= node.size()) ""
    else
      node.get(index) match {
        case s: String => s
        case n: Node   => textOf(n).getOrElse("")
        case _ =>
          try textOf(node.getNode(index)).getOrElse("")
          catch { case _: Exception => "" }
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

  private def astCode(ast: Ast): String =
    ast.root.flatMap(n => propString(n.properties.get("CODE"))).getOrElse("")

  private def propString(value: Any): Option[String] = value match {
    case null            => None
    case s: String       => Some(s)
    case Some(s: String) => Some(s)
    case Some(other)     => Some(other.toString)
    case other           => Some(other.toString)
  }

  private def isDummy(ast: Ast): Boolean =
    ast.root.exists { n =>
      propString(n.properties.get("TYPE_FULL_NAME")).exists(_.contains("dummy")) ||
        propString(n.properties.get("CODE")).exists(_.contains("dummy"))
    }
}
