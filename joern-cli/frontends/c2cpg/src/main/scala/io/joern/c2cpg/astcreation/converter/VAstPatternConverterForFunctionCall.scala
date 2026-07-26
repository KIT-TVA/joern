package io.joern.c2cpg.astcreation.converter

import io.joern.c2cpg.astcreation.{Defines, VAstCreatorNew}
import io.joern.x2cpg.Ast
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.codepropertygraph.generated.nodes.NewIdentifier
import xtc.tree.{GNode, Location, Node}

class VAstPatternConverterForFunctionCall(vAstCreator: VAstCreatorNew, converter: VAstConverter)
  extends VAstPatternConverter(
    vAstCreator,
    converter,
    List(
      "FunctionCall",
      "ExpressionStatement"
    )
  ) {

  override def convert(superCVAst: Node, converterState: VAstConverterState): Option[Seq[Ast]] = {
    // converter.getConditionalHandler.handelAndSimplifyConditional(...)
    functionCallNode(superCVAst) match {
      case None =>
        if (superCVAst.getName == "ExpressionStatement" && superCVAst.size() > 0) {
          Option(converter.convert(superCVAst.getNode(0), converterState))
        } else {
          None
        }
      case Some(callNode) =>
        val nameNode = callNode.getNode(0)
        val argsNode = callNode.getNode(1)
        val conditionalHandler = converter.getConditionalHandler
        if (conditionalHandler.isConditionalNode(nameNode)) {
          val restructuredCall: Node = conditionalHandler.createConditionalSuperCSubtree(
            nameNode,
            converterState,
            (resolvedName, state) => buildFunctionCallNode(resolvedName, argsNode)
          )
          Option(converter.convert(restructuredCall, converterState))
        } else {
          Option(Seq(buildCallAst(superCVAst, callNode, nameNode, argsNode, converterState)))
        }
    }
  }

  private def functionCallNode(node: Node): Option[Node] =
    node.getName match {
      case "FunctionCall" => Option(node)
      case "ExpressionStatement" if node.size() > 0 && node.getNode(0).getName == "FunctionCall" =>
        Option(node.getNode(0))
      case _ => None
    }

  private def buildFunctionCallNode(nameNode: Node, argsNode: Node): Node = {
    val functionCall: Node = GNode.create("FunctionCall", 2)
    functionCall.add(0, nameNode)
    functionCall.add(1, argsNode)
    functionCall
  }

  private def buildCallAst(
                            rootNode: Node,
                            callNode: Node,
                            nameNode: Node,
                            argsNode: Node,
                            converterState: VAstConverterState
                          ): Ast = {
    val name = extractFunctionName(nameNode)
    val argAsts = convertArguments(argsNode, converterState)
    val (line, column) = locationOf(rootNode)
    val code = buildCallCode(name, argAsts)
    val call = vAstCreator.callNodeHelper(
      callNode,
      code,
      name,
      name,
      DispatchTypes.STATIC_DISPATCH,
      Some(""),
      Option(Defines.Any),
      line,
      column
    )
    vAstCreator.callAst(call, argAsts)
  }

  private def buildCallCode(name: String, argAsts: Seq[Ast]): String =
    s"$name(${argAsts.map(astCode).mkString(", ")})"

  private def extractFunctionName(nameNode: Node): String =
    if (nameNode.size() > 0) nameNode.getNode(0).getString(0)
    else nameNode.getString(0)

  private def convertArguments(argsNode: Node, converterState: VAstConverterState): Seq[Ast] =
    if (argsNode.size() == 0) Seq.empty
    else {
      getChildren(argsNode).flatMap {
        case list if list.getName == "StringLiteralList" || list.getName == "ExpressionList" =>
          getChildren(list).map(argumentConverter(_, converterState))
        case node =>
          Seq(argumentConverter(node, converterState))
      }
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

  private def argumentConverter(node: Node, converterState: VAstConverterState): Ast = {
    val conditionalHandler = converter.getConditionalHandler
    if (conditionalHandler.isConditionalNode(node)) {
      if (conditionalHandler.getFirstCondition(node) == "1") {
        argumentConverter(conditionalHandler.getFirstConditionalSubtree(node), converterState)
      } else {
        val asts = conditionalHandler.handelConditional(
          node,
          converterState,
          (child, state) => Seq(argumentConverter(child, state))
        )
        asts.find(a => a.root.isDefined && !isDummyBlockAst(a)).getOrElse(vAstCreator.AstHelper())
      }
    } else {
      leafOrConvertedArgument(node, converterState)
    }
  }

  private def leafOrConvertedArgument(node: Node, converterState: VAstConverterState): Ast = {
    val converted = converter.convert(node, converterState)
    if (converted.nonEmpty && converted.head.root.isDefined && !isDummyBlockAst(converted.head)) {
      converted.head
    } else {
      node.getName match {
        case "PrimaryIdentifier"                  => identifierAst(node)
        case "superc.core.Syntax$Text"            => identifierAstFromText(node)
        case name if name.contains("Syntax$Text") => identifierAstFromText(node)
        case _ if node.size() >= 1 =>
          node.get(0) match {
            case child: Node => argumentConverter(child, converterState)
            case _           => vAstCreator.AstHelper()
          }
        case _ => vAstCreator.AstHelper()
      }
    }
  }

  private def isDummyBlockAst(ast: Ast): Boolean =
    ast.root.exists { n =>
      val props = n.properties
      codeFromProperty(props.get("CODE")).exists(_.contains("dummy block")) ||
        codeFromProperty(props.get("TYPE_FULL_NAME")).exists(_.contains("dummy block"))
    }

  private def identifierAst(node: Node): Ast = {
    val name = firstStringChild(node)
    identifierAstFromName(node, name)
  }

  private def identifierAstFromText(node: Node): Ast = {
    identifierAstFromName(node, firstStringChild(node))
  }

  private def firstStringChild(node: Node): String = {
    var i = 0
    while (i < node.size()) {
      node.get(i) match {
        case value: String => return value
        case child: Node =>
          val nested = firstStringChild(child)
          if (nested.nonEmpty) return nested
        case _ =>
      }
      i += 1
    }
    ""
  }

  private def identifierAstFromName(node: Node, name: String): Ast = {
    val (line, column) = locationOf(node)
    val id = NewIdentifier()
      .name(name)
      .code(name)
      .typeFullName(Defines.Any)
      .lineNumber(line)
      .columnNumber(column)
    vAstCreator.AstHelper(id)
  }

  private def locationOf(node: Node): (Option[Int], Option[Int]) = {
    val loc: Location = node.getLocation
    if (loc == null) (None, None) else (Option(loc.line), Option(loc.column))
  }

  private def astCode(ast: Ast): String =
    ast.root.flatMap(n => codeFromProperty(n.properties.get("CODE"))).getOrElse("")

  private def codeFromProperty(value: Any): Option[String] = value match {
    case null            => None
    case s: String       => Some(s)
    case Some(s: String) => Some(s)
    case Some(other)     => Some(other.toString)
    case other           => Some(other.toString)
  }
}
