package io.joern.c2cpg.astcreation.converter

import io.joern.c2cpg.astcreation.VAstCreatorNew
import io.joern.x2cpg.Ast
import xtc.tree.{Location, Node}

/** SuperC: IterationStatement for `while` and `do-while` (Task 19). */
class VAstPatternConverterForWhileLoop(vAstCreator: VAstCreatorNew, converter: VAstConverter)
  extends VAstPatternConverter(
    vAstCreator,
    converter,
    List("IterationStatement")
  ) {

  override def convert(superCVAst: Node, converterState: VAstConverterState): Option[Seq[Ast]] = {
    if (superCVAst.size() < 3) None
    else {
      val ast = keywordAt(superCVAst, 0) match {
        case "while" => convertWhile(superCVAst, converterState)
        case "do"    => convertDoWhile(superCVAst, converterState)
        case _       => None
      }
      ast.map(a => Seq(a))
    }
  }

  private def convertWhile(iterationNode: Node, converterState: VAstConverterState): Option[Ast] = {
    if (iterationNode.size() < 3) None
    else {
      val conditionAst = convertSubtree(iterationNode.getNode(1), converterState)
      val bodyAst      = convertBody(iterationNode.getNode(2), converterState)
      if (conditionAst.root.isEmpty) None
      else {
        val (line, column) = locationOf(iterationNode)
        val code           = s"while (${astCode(conditionAst)})"
        Option(
          vAstCreator.whileAst(
            Option(conditionAst),
            Seq(bodyAst),
            code = Option(code),
            lineNumber = line,
            columnNumber = column
          )
        )
      }
    }
  }

  private def convertDoWhile(iterationNode: Node, converterState: VAstConverterState): Option[Ast] = {
    if (iterationNode.size() < 3) None
    else {
      val bodyAst = convertBody(iterationNode.getNode(1), converterState)
      val conditionNode =
        if (iterationNode.size() >= 4 && keywordAt(iterationNode, 2) == "while") iterationNode.getNode(3)
        else iterationNode.getNode(2)
      val conditionAst = convertSubtree(conditionNode, converterState)
      if (conditionAst.root.isEmpty) None
      else {
        val (line, column) = locationOf(iterationNode)
        val code           = s"do { ${bodyCode(bodyAst)} } while (${astCode(conditionAst)})"
        Option(
          vAstCreator.doWhileAst(
            Option(conditionAst),
            Seq(bodyAst),
            code = Option(code),
            lineNumber = line,
            columnNumber = column
          )
        )
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

  private def convertSubtree(node: Node, converterState: VAstConverterState): Ast =
    converter.convert(node, converterState).headOption.getOrElse(vAstCreator.AstHelper())

  private def keywordAt(node: Node, index: Int): String =
    if (index >= node.size()) ""
    else
      node.get(index) match {
        case value: String => value
        case _             => Option(node.getNode(index)).map(_.toString).getOrElse("")
      }

  private def getChildren(node: Node): Seq[Node] =
    (0 until node.size()).flatMap { i =>
      node.get(i) match {
        case child: Node => Some(child)
        case _ =>
          try Option(node.getNode(i))
          catch { case _: Exception => None }
      }
    }

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
