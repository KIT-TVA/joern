package io.joern.c2cpg.astcreation.converter

import io.joern.c2cpg.astcreation.VAstCreatorNew
import io.joern.x2cpg.Ast
import io.shiftleft.codepropertygraph.generated.ControlStructureTypes
import io.shiftleft.codepropertygraph.generated.nodes.NewLocal
import xtc.tree.{Location, Node}

/** SuperC: IterationStatement / ForStatement for `for (...; ...; ...) { ... }` (Task 20). */
class VAstPatternConverterForForLoop(vAstCreator: VAstCreatorNew, converter: VAstConverter)
  extends VAstPatternConverter(
    vAstCreator,
    converter,
    List(
      "IterationStatement",
      "ForStatement"
    )
  ) {

  override def convert(superCVAst: Node, converterState: VAstConverterState): Option[Seq[Ast]] = {
    if (!isForNode(superCVAst)) None
    else convertFor(superCVAst, converterState).map(Seq(_))
  }

  private def isForNode(node: Node): Boolean =
    node.getName == "ForStatement" ||
      (node.getName == "IterationStatement" && keywordAt(node, 0) == "for")

  private def convertFor(forNode: Node, converterState: VAstConverterState): Option[Ast] = {
    val parts = forParts(forNode)
    if (parts.isEmpty) None
    else {
      val (initNode, condNode, updateNode, bodyNode) = parts.get
      val initAsts = convertOptional(initNode, converterState)
      val (localAsts, plainInitAsts) =
        initAsts.partition(_.root.exists(_.isInstanceOf[NewLocal]))
      val conditionAsts = convertOptional(condNode, converterState)
      val updateAsts    = convertOptional(updateNode, converterState)
      val bodyAst       = convertBody(bodyNode, converterState)

      val (line, column) = locationOf(forNode)
      val code = s"for (${astCodeSeq(plainInitAsts)}; ${astCodeSeq(conditionAsts)}; ${astCodeSeq(updateAsts)})"
      val controlNode =
        vAstCreator.controlStructureNodeHelper(forNode, ControlStructureTypes.FOR, code, line, column)
      Option(
        vAstCreator.forAst(
          controlNode,
          localAsts,
          plainInitAsts,
          conditionAsts,
          updateAsts,
          bodyAst
        )
      )
    }
  }

  /**
   * From Fynn's SuperC VAST (ForStatement):
   *   [0] init  — Declaration / Expression  (e.g. int i = 0)
   *   [1] cond  — Expression               (e.g. i < 42)
   *   [2] update — Increment / Expression  (e.g. i++)
   *   [3] body  — CompoundStatement
   * IterationStatement with keyword "for" may still prefix the keyword at [0].
   */
  private def forParts(node: Node): Option[(Option[Node], Option[Node], Option[Node], Node)] = {
    if (node.getName == "ForStatement" && node.size() >= 4) {
      Some((safeNodeAt(node, 0), safeNodeAt(node, 1), safeNodeAt(node, 2), node.getNode(3)))
    } else {
      val offset = if (keywordAt(node, 0) == "for") 1 else 0
      val bodyIndex = node.size() - 1
      if (bodyIndex < offset) None
      else {
        val body = safeNodeAt(node, bodyIndex)
        if (body.isEmpty) None
        else {
          val middle = (offset until bodyIndex).flatMap(i => safeNodeAt(node, i))
          middle.size match {
            case 0 => Some((None, None, None, body.get))
            case 1 => Some((Some(middle(0)), None, None, body.get))
            case 2 => Some((Some(middle(0)), Some(middle(1)), None, body.get))
            case _ => Some((Some(middle(0)), Some(middle(1)), Some(middle(2)), body.get))
          }
        }
      }
    }
  }

  private def convertOptional(nodeOpt: Option[Node], converterState: VAstConverterState): Seq[Ast] =
    nodeOpt.map(n => convertAll(n, converterState)).getOrElse(Seq.empty)

  private def convertAll(node: Node, converterState: VAstConverterState): Seq[Ast] = {
    val name = node.getName
    // Empty optional slots / punctuation placeholders
    if (name == null || name.isEmpty || name == ";" || name.contains("Empty")) Seq.empty
    else {
      val converted = converter.convert(node, converterState)
      if (converted.nonEmpty) converted
      else {
        // InitialClause / ExpressionStatement wrappers: try children
        getChildren(node).flatMap(child => converter.convert(child, converterState))
      }
    }
  }

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

  private def locationOf(node: Node): (Option[Int], Option[Int]) = {
    val loc: Location = node.getLocation
    if (loc == null) (None, None) else (Option(loc.line), Option(loc.column))
  }

  private def astCode(ast: Ast): String =
    ast.root.flatMap(n => codeFromProperty(n.properties.get("CODE"))).getOrElse("")

  private def astCodeSeq(asts: Seq[Ast]): String =
    asts.map(astCode).filter(_.nonEmpty).mkString(" ")

  private def codeFromProperty(value: Any): Option[String] = value match {
    case null            => None
    case s: String       => Some(s)
    case Some(s: String) => Some(s)
    case Some(other)     => Some(other.toString)
    case other           => Some(other.toString)
  }
}
