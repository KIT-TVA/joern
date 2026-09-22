package io.joern.c2cpg.astcreation.converter

import io.joern.c2cpg.astcreation.{Defines, VAstCreatorNew}
import io.joern.x2cpg.Ast
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.{NewIdentifier, NewLiteral}
import xtc.tree.{Location, Node}

/**
 * SuperC Task 7: plain `enum` → TYPE_DECL + MEMBER (+ optional `green = 20` assignment).
 *
 * SuperC shape (from test_enum):
 *   Declaration → SUETypeSpecifier → EnumSpecifier
 *     [0] Language("enum")
 *     [1] IdentifierOrTypedefName(Text("color"))
 *     [2] EnumeratorList
 *           Conditional(1, Enumerator(Text(name), EnumeratorValueOpt(Text(value)?)))
 *
 * Type-only uses (`enum color c`) have EnumSpecifier without EnumeratorList → ignored.
 */
class VAstPatternConverterForEnum(vAstCreator: VAstCreatorNew, converter: VAstConverter)
  extends VAstPatternConverter(
    vAstCreator,
    converter,
    List("Declaration", "SUETypeSpecifier", "EnumSpecifier")
  ) {

  override def convert(superCVAst: Node, converterState: VAstConverterState): Option[Seq[Ast]] = {
    findEnumDefinition(superCVAst).flatMap(n => convertEnum(n, converterState).map(Seq(_)))
  }

  /** Only definitions with EnumeratorList (not `enum color` as a type name). */
  private def findEnumDefinition(node: Node): Option[Node] = {
    Option(node.getName).getOrElse("") match {
      case "EnumSpecifier" if hasEnumeratorList(node) => Some(node)
      case "SUETypeSpecifier" =>
        safeNodeAt(node, 0).filter(n => n.getName == "EnumSpecifier" && hasEnumeratorList(n))
      case "Declaration" =>
        safeNodeAt(node, 0).flatMap(findEnumDefinition)
      case _ => None
    }
  }

  private def hasEnumeratorList(enumNode: Node): Boolean =
    getChildren(enumNode).exists(_.getName == "EnumeratorList")

  private def convertEnum(enumNode: Node, converterState: VAstConverterState): Option[Ast] = {
    val typeName   = enumName(enumNode).getOrElse("<enum>")
    val memberAsts = getChildren(enumNode)
      .find(_.getName == "EnumeratorList")
      .toSeq
      .flatMap(list => getChildren(list).flatMap(c => convertOne(c, typeName, converterState)))

    val (line, column) = locationOf(enumNode)
    val code           = s"enum $typeName { ... }"
    val typeDecl = vAstCreator.typeDeclNodeHelper(
      enumNode,
      typeName,
      typeName,
      vAstCreator.getCurrentFilename,
      code,
      line,
      column
    )
    Option(vAstCreator.AstHelper(typeDecl).withChildren(memberAsts))
  }

  private def enumName(enumNode: Node): Option[String] =
    getChildren(enumNode)
      .find(n => n.getName == "IdentifierOrTypedefName" || n.getName.contains("Identifier"))
      .flatMap(textOf)

  private def convertOne(node: Node, enumTypeName: String, converterState: VAstConverterState): Seq[Ast] = {
    val h = converter.getConditionalHandler
    if (h.isConditionalNode(node)) {
      if (h.getFirstCondition(node) == "1") convertOne(h.getFirstConditionalSubtree(node), enumTypeName, converterState)
      else h.handelConditional(node, converterState, (n, s) => convertOne(n, enumTypeName, s))
    } else if (node.getName == "Enumerator") {
      val memberName = safeNodeAt(node, 0).flatMap(textOf).getOrElse("")
      if (memberName.isEmpty) Seq.empty
      else {
        val valueOpt = safeNodeAt(node, 1).flatMap(enumeratorValue)
        val (line, column) = locationOf(node)
        val code           = valueOpt.map(v => s"$memberName = $v").getOrElse(memberName)
        val member =
          vAstCreator.AstHelper(vAstCreator.memberNodeHelper(node, memberName, code, enumTypeName, line, column))
        valueOpt match {
          case Some(v) => Seq(member, assignmentAst(node, memberName, v, line, column))
          case None    => Seq(member)
        }
      }
    } else Seq.empty
  }

  /** EnumeratorValueOpt → optional Text("20"). */
  private def enumeratorValue(optNode: Node): Option[String] =
    getChildren(optNode).flatMap(textOf).find(_.matches("-?[0-9]+"))

  private def assignmentAst(
                             node: Node,
                             memberName: String,
                             value: String,
                             line: Option[Int],
                             column: Option[Int]
                           ): Ast = {
    val call = vAstCreator.callNodeHelper(
      node,
      s"$memberName = $value",
      Operators.assignment,
      Operators.assignment,
      DispatchTypes.STATIC_DISPATCH,
      None,
      Some(Defines.Void),
      line,
      column
    )
    val left = NewIdentifier()
      .name(memberName)
      .code(memberName)
      .typeFullName(Defines.Any)
      .lineNumber(line)
      .columnNumber(column)
    val right = NewLiteral().code(value).typeFullName("int").lineNumber(line).columnNumber(column)
    vAstCreator.callAst(call, List(vAstCreator.AstHelper(left), vAstCreator.AstHelper(right)))
  }

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
        case s: String if s.nonEmpty => return s
        case n: Number               => return n.toString
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
