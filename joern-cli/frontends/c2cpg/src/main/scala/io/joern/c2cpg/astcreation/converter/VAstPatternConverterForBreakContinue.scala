package io.joern.c2cpg.astcreation.converter

import io.joern.c2cpg.astcreation.VAstCreatorNew
import io.joern.x2cpg.Ast
import io.shiftleft.codepropertygraph.generated.ControlStructureTypes
import xtc.tree.{Location, Node}

/** SuperC: BreakStatement / ContinueStatement (and JumpStatement) — Task 21. */
class VAstPatternConverterForBreakContinue(vAstCreator: VAstCreatorNew, converter: VAstConverter)
  extends VAstPatternConverter(
    vAstCreator,
    converter,
    List(
      "BreakStatement",
      "ContinueStatement",
      "JumpStatement"
    )
  ) {

  override def convert(superCVAst: Node, converterState: VAstConverterState): Option[Seq[Ast]] = {
    jumpKind(superCVAst) match {
      case None => None
      case Some((controlType, code)) =>
        val (line, column) = locationOf(superCVAst)
        val node = vAstCreator.controlStructureNodeHelper(superCVAst, controlType, code, line, column)
        Option(Seq(vAstCreator.AstHelper(node)))
    }
  }

  private def jumpKind(node: Node): Option[(String, String)] =
    node.getName match {
      case "BreakStatement"    => Some((ControlStructureTypes.BREAK, "break;"))
      case "ContinueStatement" => Some((ControlStructureTypes.CONTINUE, "continue;"))
      case "JumpStatement" =>
        keywordAt(node, 0) match {
          case "break"    => Some((ControlStructureTypes.BREAK, "break;"))
          case "continue" => Some((ControlStructureTypes.CONTINUE, "continue;"))
          case _          => None
        }
      case _ => None
    }

  private def keywordAt(node: Node, index: Int): String =
    if (index >= node.size()) ""
    else
      node.get(index) match {
        case value: String => value
        case _             => Option(node.getNode(index)).map(_.toString).getOrElse("")
      }

  private def locationOf(node: Node): (Option[Int], Option[Int]) = {
    val loc: Location = node.getLocation
    if (loc == null) (None, None) else (Option(loc.line), Option(loc.column))
  }
}
