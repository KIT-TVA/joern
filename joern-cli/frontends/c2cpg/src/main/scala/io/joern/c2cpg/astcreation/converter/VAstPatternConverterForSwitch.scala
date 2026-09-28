package io.joern.c2cpg.astcreation.converter

import io.joern.c2cpg.astcreation.{Defines, VAstCreatorNew}
import io.joern.x2cpg.{Ast, AstEdge}
import io.shiftleft.codepropertygraph.generated.nodes.{NewIdentifier, NewLiteral, NewNode}
import io.shiftleft.codepropertygraph.generated.ControlStructureTypes
import xtc.tree.{Location, Node}

import scala.collection.mutable.ListBuffer

/**
 * SuperC: `switch` / `case` / `default` (Task 13).
 *
 * Joern: SWITCH + condition; body with JUMP_TARGET (+ case expr) / default / stmts.
 * Instructions between a label and the next case/default or a break go into one BLOCK.
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

  private val conditionalHandler: VAstConditionalHandler = converter.getConditionalHandler

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
  private def convertCase(caseNode: Node, converterState: VAstConverterState): Seq[Ast] =
    labelAsts(caseNode, "case", converterState) ++ nestedStmt(caseNode, converterState)

  /** JUMP_TARGET plus the case value, without the statements of the section. */
  private def labelAsts(labelNode: Node, kind: String, converterState: VAstConverterState): Seq[Ast] =
    if (kind == "default") {
      val (line, column) = locationOf(labelNode)
      Seq(vAstCreator.AstHelper(vAstCreator.jumpTargetNodeHelper(labelNode, "default", "default:", line, column)))
    } else
      conditionalCaseValueNode(labelNode) match {
        case Some(cond) =>
          conditionalHandler.handleConditional(
            cond,
            converterState,
            (resolvedValue, state) => convertCaseLabelOnly(labelNode, textOf(resolvedValue)).filter(isJumpTarget).take(1)
          )
        case None => convertCaseLabelOnly(labelNode, caseValue(labelNode)).filter(isJumpTarget)
      }

  private def labelKind(node: Node): Option[String] =
    Option(node.getName).getOrElse("") match {
      case "CaseStatement" | "CaseLabeledStatement"       => Option("case")
      case "DefaultStatement" | "DefaultLabeledStatement" => Option("default")
      case "LabeledStatement" =>
        keywordAt(node, 0) match {
          case kw if kw.contains("case") && !kw.contains("cased") => Option("case")
          case kw if kw.contains("default")                       => Option("default")
          case _                                                  => None
        }
      case _ => None
    }

  private def convertCaseLabelOnly(caseNode: Node, value: Option[String]): Seq[Ast] = {
    val (line, column) = locationOf(caseNode)
    val exprAst        = value.map(v => leafAst(v, line, column))
    val code           = value.map(v => s"case $v:").getOrElse("case:")
    val jump =
      vAstCreator.AstHelper(vAstCreator.jumpTargetNodeHelper(caseNode, "case", code, line, column))
    Seq(jump) ++ exprAst.toSeq
  }

  /** Real `#ifdef` on the case label value (not Conditional("1", …)). */
  private def conditionalCaseValueNode(caseNode: Node): Option[Node] = {
    val start = if (keywordAt(caseNode, 0).contains("case")) 1 else 0
    (start until caseNode.size()).view
      .flatMap(i => safeNodeAt(caseNode, i))
      .find { n =>
        conditionalHandler.isSuperCConditionalNode(n) &&
          conditionalHandler.getFirstSuperCCondition(n) != "1"
      }
  }

  private def convertDefault(defaultNode: Node, converterState: VAstConverterState): Seq[Ast] =
    labelAsts(defaultNode, "default", converterState) ++ nestedStmt(defaultNode, converterState)

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
          if (conditionalHandler.isSuperCConditionalNode(n) && conditionalHandler.getFirstSuperCCondition(n) == "1")
            textOf(conditionalHandler.getFirstSuperCConditionalSubtree(n))
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
    if (conditionalHandler.isSuperCConditionalNode(bodyNode)
      && conditionalHandler.getFirstSuperCCondition(bodyNode) == "1") {
      convertBody(conditionalHandler.getFirstSuperCConditionalSubtree(bodyNode), converterState)
    } else if (conditionalHandler.isSuperCConditionalNode(bodyNode)) {
      wrapInBlock(bodyNode, conditionalHandler.handleConditional(bodyNode, converterState, (n, s) => convertChild(n, s)))
    } else if (bodyNode.getName == "CompoundStatement" && bodyNode.size() >= 2) {
      wrapInBlock(bodyNode, convertStatementList(bodyNode.getNode(1), converterState))
    } else {
      wrapInBlock(bodyNode, convertChild(bodyNode, converterState).filterNot(isDummy))
    }
  }

  /**
   * A section starts at a case/default label and ends at the next label or at a break.
   * The label and the break stay outside the BLOCK.
   */
  private def convertStatementList(listNode: Node, converterState: VAstConverterState): Seq[Ast] = {
    val bodyAsts: ListBuffer[Ast]    = ListBuffer.empty[Ast]
    val sectionAsts: ListBuffer[Ast] = ListBuffer.empty[Ast]
    var sectionAnchor: Node          = listNode
    var insideSection: Boolean       = false

    def closeSection(): Unit = if (sectionAsts.nonEmpty) {
      bodyAsts += caseBodyBlock(sectionAnchor, sectionAsts.toSeq)
      sectionAsts.clear()
    }

    for (child: Node <- getChildren(listNode)) {
      val statementNode: Node = unwrapTrivialConditional(child)
      if (conditionalHandler.isSuperCConditionalNode(statementNode) && firstLabel(statementNode).isDefined) {
        // `#ifdef` on the label. CHOICE carries only the JUMP_TARGETs.
        // Statements that follow the label go into the section BLOCK; break stays after it.
        closeSection()
        bodyAsts ++= labelChoice(statementNode, converterState)
        val labelNode: Node = firstLabel(statementNode).get
        val secondIsLabel: Boolean =
          conditionalHandler.getSecondSuperCConditionalSubtree(statementNode).exists(n => firstLabel(n).isDefined)
        if (secondIsLabel) {
          // case 0 / case 1: the body is shared, so it sits once, after the CHOICE.
          sectionAnchor = labelNode
          insideSection = true
          sectionAsts ++= bodyPieces(nestedStmt(labelNode, converterState))
        } else {
          // optional case: the body is conditional too, as its own BLOCK under the same condition.
          bodyAsts ++= bodyChoice(statementNode, converterState)
          insideSection = false
        }
      } else labelKind(statementNode) match {
        case Some(kind) =>
          closeSection()
          val labels: Seq[Ast] = labelAsts(statementNode, kind, converterState)
          bodyAsts ++= labels.filter(ast => isJumpTarget(ast) || isChoice(ast))
          sectionAnchor = statementNode
          insideSection = true
          sectionAsts ++= bodyPieces(nestedStmt(statementNode, converterState))
        case None =>
          val statementAsts: Seq[Ast] = convertChild(child, converterState).filterNot(isDummy)
          if (statementAsts.exists(isBreak)) {
            closeSection()
            bodyAsts ++= statementAsts
            insideSection = false
          } else if (insideSection) sectionAsts ++= statementAsts.flatMap(explodeBlock).filterNot(isJumpTarget)
          else bodyAsts ++= statementAsts
      }
    }
    closeSection()
    bodyAsts.toSeq
  }

  private def caseBodyBlock(anchor: Node, stmts: Seq[Ast]): Ast = {
    val kept: Seq[Ast] = stmts.filter(_.root.isDefined)
    val (line, column) = firstPosition(kept).getOrElse(locationOf(anchor))
    val code           = kept.map(astCode).filter(_.nonEmpty).mkString("\n")
    val block =
      vAstCreator.blockNodeHelper(anchor, if (code.nonEmpty) code else "<empty>", "<???>", line, column)
    vAstCreator.blockAstHelper(block, kept.toList)
  }

  private def firstPosition(stmts: Seq[Ast]): Option[(Option[Int], Option[Int])] =
    stmts.view
      .flatMap(_.root)
      .map(node => (propInt(node.properties.get("LINE_NUMBER")), propInt(node.properties.get("COLUMN_NUMBER"))))
      .find { case (line, _) => line.isDefined }

  /** First branch that is a case/default label, skipping Conditional("1"). */
  private def firstLabel(node: Node): Option[Node] = {
    val current: Node = unwrapTrivialConditional(node)
    labelKind(current) match {
      case Some(_) => Some(current)
      case None if conditionalHandler.isSuperCConditionalNode(current) =>
        firstLabel(conditionalHandler.getFirstSuperCConditionalSubtree(current))
      case None => None
    }
  }

  /** CHOICE whose branches are only the JUMP_TARGET, not the case body. */
  private def labelChoice(node: Node, converterState: VAstConverterState): Seq[Ast] =
    conditionalHandler.handleConditional(node, converterState, (branch, state) =>
      firstLabel(branch) match {
        case Some(label) =>
          val labels: Seq[Ast] = labelAsts(label, labelKind(label).get, state)
          val choices: Seq[Ast] = labels.filter(isChoice)
          if (choices.nonEmpty) choices else labels.filter(isJumpTarget).take(1)
        case None        => Seq(caseBodyBlock(branch, Seq.empty))
      }
    ).filterNot(isDummy)

  /** Same condition as the label, but the branch is the body BLOCK (no JUMP_TARGET, no break). */
  private def bodyChoice(node: Node, converterState: VAstConverterState): Seq[Ast] =
    conditionalHandler.handleConditional(node, converterState, (branch, state) => {
      val stmts: Seq[Ast] = firstLabel(branch).toSeq.flatMap(label => bodyPieces(nestedStmt(label, state)))
      Seq(caseBodyBlock(branch, stmts))
    }).filterNot(isDummy)

  /** Drop the label and a trailing break; keep the instructions that belong in the section BLOCK. */
  private def bodyPieces(asts: Seq[Ast]): Seq[Ast] =
    asts.flatMap(explodeBlock).filterNot(ast => isJumpTarget(ast) || isBreak(ast)).filterNot(isDummy)

  /** A BLOCK that starts with the case label is split so the label can sit in front of it. */
  private def explodeBlock(ast: Ast): Seq[Ast] =
    if (isBlock(ast) && directChildAsts(ast).exists(isJumpTarget)) directChildAsts(ast).flatMap(explodeBlock)
    else Seq(ast)

  private def isBlock(ast: Ast): Boolean =
    ast.root.exists(_.label == "BLOCK")

  private def isJumpTarget(ast: Ast): Boolean =
    ast.root.exists { node =>
      propString(node.properties.get("NAME")).exists(name => name == "case" || name == "default")
    }

  private def isChoice(ast: Ast): Boolean =
    ast.root.exists(node =>
      propString(node.properties.get("CONTROL_STRUCTURE_TYPE")).contains(ControlStructureTypes.CHOICE)
    )

  private def directChildAsts(ast: Ast): Seq[Ast] = ast.root.toSeq.flatMap { root =>
    ast.edges.collect { case edge if edge.src == root => edge.dst }.map { child =>
      val reached = scala.collection.mutable.Set[NewNode](child)
      var grew = true
      while (grew) {
        grew = false
        ast.edges.foreach { edge =>
          if (reached.contains(edge.src) && reached.add(edge.dst)) grew = true
        }
      }
      def keep(edges: collection.Seq[AstEdge]): collection.Seq[AstEdge] =
        edges.filter(edge => reached.contains(edge.src) && reached.contains(edge.dst))
      Ast(
        nodes = child +: ast.nodes.filter(node => reached.contains(node) && (node ne child)).toSeq,
        edges = keep(ast.edges),
        conditionEdges = keep(ast.conditionEdges),
        refEdges = keep(ast.refEdges),
        bindsEdges = keep(ast.bindsEdges),
        receiverEdges = keep(ast.receiverEdges),
        argEdges = keep(ast.argEdges),
        captureEdges = keep(ast.captureEdges)
      )
    }
  }

  private def unwrapTrivialConditional(node: Node): Node =
    if (conditionalHandler.isSuperCConditionalNode(node)
      && conditionalHandler.getFirstSuperCCondition(node) == "1") {
      unwrapTrivialConditional(conditionalHandler.getFirstSuperCConditionalSubtree(node))
    } else node

  private def isBreak(ast: Ast): Boolean =
    ast.root.exists(node =>
      propString(node.properties.get("CONTROL_STRUCTURE_TYPE")).contains(ControlStructureTypes.BREAK)
    )

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
    if (conditionalHandler.isSuperCConditionalNode(node)) {
      if (conditionalHandler.getFirstSuperCCondition(node) == "1") {
        convertChild(conditionalHandler.getFirstSuperCConditionalSubtree(node), converterState)
      } else conditionalHandler.handleConditional(node, converterState, (n, s) => convertChild(n, s))
    } else if (node.getName == "CompoundStatement") {
      Seq(convertBody(node, converterState))
    } else {
      converter.convert(node, converterState)
    }
  }

  private def convertExpr(node: Node, converterState: VAstConverterState): Ast = {
    if (conditionalHandler.isSuperCConditionalNode(node)) {
      if (conditionalHandler.getFirstSuperCCondition(node) == "1") {
        convertExpr(conditionalHandler.getFirstSuperCConditionalSubtree(node), converterState)
      } else {
        // Do not fall back to textOf(Conditional) — that collapses `#ifdef` to one IDENTIFIER.
        val asts = conditionalHandler.handleConditional(
          node,
          converterState,
          (child, state) => Seq(convertExpr(child, state))
        )
        asts.find(a => a.root.isDefined && !isDummy(a)).getOrElse(vAstCreator.AstHelper())
      }
    } else {
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
    else extractQuotedName(node.toString)
  }

  /** Avoid RegExp escapes that break on newer JDKs (`\]` etc.). */
  private def extractQuotedName(text: String): Option[String] = {
    def between(open: String, close: String): Option[String] = {
      val i = text.indexOf(open)
      if (i < 0) None
      else {
        val start = i + open.length
        val j     = text.indexOf(close, start)
        if (j <= start) None else Option(text.substring(start, j)).filter(_.nonEmpty)
      }
    }
    between("[\"", "\"]").orElse(between("(\"", "\")"))
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

  private def propInt(value: Any): Option[Int] = value match {
    case null              => None
    case number: Int       => Option(number)
    case Some(number: Int) => Option(number)
    case Some(other)       => propInt(other)
    case other             => scala.util.Try(other.toString.toInt).toOption
  }

  private def isDummy(ast: Ast): Boolean =
    ast.root.exists { n =>
      propString(n.properties.get("TYPE_FULL_NAME")).exists(_.contains("dummy")) ||
        propString(n.properties.get("CODE")).exists(_.contains("dummy"))
    }
}
