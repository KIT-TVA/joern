package io.joern.c2cpg.astcreation.converter

import io.joern.c2cpg.astcreation.VAstCreatorNew
import io.joern.x2cpg.Ast
import io.shiftleft.codepropertygraph.generated.ControlStructureTypes
import xtc.tree.{Location, Node}

/** SuperC: SelectionStatement for `if` / `else if` / `else` (Task 12). */
class VAstPatternConverterForIf(vAstCreator: VAstCreatorNew, converter: VAstConverter)
  extends VAstPatternConverter(
    vAstCreator,
    converter,
    List("SelectionStatement")
  ) {

  override def convert(superCVAst: Node, converterState: VAstConverterState): Option[Seq[Ast]] = {
    if (keywordAt(superCVAst, 0) != "if" || superCVAst.size() < 3) None
    else convertIf(superCVAst, converterState).map(Seq(_))
  }

  /**
   * SuperC VAST (SelectionStatement), aligned with legacy `VAstCreator.astForIf`:
   *   [0] "if"
   *   [1] condition expression
   *   [2] then branch (statement or CompoundStatement)
   *   [3] "else" keyword (optional)
   *   [4] else branch (optional; may be nested SelectionStatement for else-if)
   *
   * Some SuperC builds omit the keyword slot and place the else branch at [3].
   */
  private def convertIf(ifNode: Node, converterState: VAstConverterState): Option[Ast] = {
    val conditionAst = convertSubtree(ifNode.getNode(1), converterState)
    if (conditionAst.root.isEmpty) None
    else {
      val thenAst = convertBody(ifNode.getNode(2), converterState)
      val (line, column) = locationOf(ifNode)
      val elseBodyOpt = elseBodyNode(ifNode).map(convertBody(_, converterState))
      val elseWrapped = elseBodyOpt.map { elseBodyAst =>
        val elseControlNode =
          vAstCreator.controlStructureNodeHelper(ifNode, ControlStructureTypes.ELSE, "else", line, column)
        vAstCreator.AstHelper(elseControlNode).withChild(elseBodyAst)
      }

      val condCode = astCode(conditionAst)
      val code = elseBodyOpt match {
        case Some(elseBody) =>
          s"if ($condCode) { ${bodyCode(thenAst)} } else { ${bodyCode(elseBody)} }"
        case None =>
          s"if ($condCode) { ${bodyCode(thenAst)} }"
      }

      val ifControlNode =
        vAstCreator.controlStructureNodeHelper(ifNode, ControlStructureTypes.IF, code, line, column)
      val children = elseWrapped match {
        case Some(e) => Seq(thenAst, e)
        case None    => Seq(thenAst)
      }
      Option(vAstCreator.controlStructureAst(ifControlNode, Option(conditionAst), children))
    }
  }

  private def elseBodyNode(ifNode: Node): Option[Node] =
    if (ifNode.size() >= 5 && keywordAt(ifNode, 3) == "else") safeNodeAt(ifNode, 4)
    else if (ifNode.size() == 4 && keywordAt(ifNode, 3) != "else") safeNodeAt(ifNode, 3)
    else None

  private def convertBody(bodyNode: Node, converterState: VAstConverterState): Ast =
    if (bodyNode.getName == "CompoundStatement" && bodyNode.size() >= 2) {
      val stmtAsts = getChildren(bodyNode.getNode(1)).flatMap { child =>
        converter.convert(child, converterState)
      }
      val (line, column) = locationOf(bodyNode)
      val code           = stmtAsts.map(astCode).filter(_.nonEmpty).mkString("\n")
      val block = vAstCreator.blockNodeHelper(
        bodyNode,
        if (code.nonEmpty) code else "<empty>",
        "<???>",
        line,
        column
      )
      vAstCreator.blockAstHelper(block, stmtAsts.toList)
    } else {
      converter.convert(bodyNode, converterState).headOption.getOrElse(vAstCreator.AstHelper())
    }

  private def convertSubtree(node: Node, converterState: VAstConverterState): Ast =
    converter.convert(node, converterState).headOption.getOrElse(vAstCreator.AstHelper())

  private def keywordAt(node: Node, index: Int): String =
    if (index >= node.size()) ""
    else
      node.get(index) match {
        case value: String => value
        case _             => Option(node.getNode(index)).map(_.toString).getOrElse("")
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

  private def bodyCode(ast: Ast): String =
    ast.root.flatMap(n => codeFromProperty(n.properties.get("CODE"))).getOrElse("")

  private def astCode(ast: Ast): String =
    ast.root.flatMap(n => codeFromProperty(n.properties.get("CODE"))).getOrElse("")

  private def locationOf(node: Node): (Option[Int], Option[Int]) = {
    val loc: Location = node.getLocation
    if (loc == null) (None, None) else (Option(loc.line), Option(loc.column))
  }

  private def codeFromProperty(value: Any): Option[String] = value match {
    case null            => None
    case s: String       => Some(s)
    case Some(s: String) => Some(s)
    case Some(other)     => Some(other.toString)
    case other           => Some(other.toString)
  }
}
