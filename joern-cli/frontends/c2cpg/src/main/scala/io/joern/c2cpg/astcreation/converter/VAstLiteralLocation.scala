package io.joern.c2cpg.astcreation.converter

import xtc.tree.{Location, Node}

/**
 * Line and column live on the syntax node that holds the first C literal of a feature.
 * The feature node itself has no location.
 */
object VAstLiteralLocation {

  def of(node: Node): (Option[Int], Option[Int]) =
    if (node == null) (None, None)
    else read(node).getOrElse((None, None))

  private def read(node: Node): Option[(Option[Int], Option[Int])] = {
    val own =
      if (hasStringChild(node)) {
        val loc: Location = node.getLocation
        if (loc == null) None else Some((Option(loc.line), Option(loc.column)))
      } else None
    if (own.isDefined) own
    else {
      var index = 0
      var found: Option[(Option[Int], Option[Int])] = None
      while (index < node.size() && found.isEmpty) {
        childNode(node, index).foreach(child => found = read(child))
        index += 1
      }
      found
    }
  }

  private def hasStringChild(node: Node): Boolean = {
    var index = 0
    var found = false
    while (index < node.size() && !found) {
      found = try {
        node.get(index).isInstanceOf[String]
      } catch {
        case _: Exception => false
      }
      index += 1
    }
    found
  }

  private def childNode(node: Node, index: Int): Option[Node] =
    try {
      node.get(index) match {
        case child: Node => Some(child)
        case _           => None
      }
    } catch {
      case _: Exception => None
    }
}
