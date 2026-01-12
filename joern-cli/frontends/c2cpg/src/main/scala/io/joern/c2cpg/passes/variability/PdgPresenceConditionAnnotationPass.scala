package io.joern.c2cpg.passes.variability

import flatgraph.{Edge, GNode}
import io.circe.parser.decode
import io.circe.syntax.*
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{ControlStructure, Method, StoredNode}
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.shiftleft.semanticcpg.language.*

/** A pass that creates control flow graphs from abstract syntax trees.
 *
 * Control flow graphs can be calculated independently per method. Therefore, we inherit from
 * `ForkJoinParallelCpgPass`.
 *
 * Note: the version of OverflowDB that we currently use as a storage backend does not assign ids to edges and this
 * pass only creates edges at the moment. Therefore, we currently do without key pools.
 */
class PdgPresenceConditionAnnotationPass(cpg: Cpg) extends ForkJoinParallelCpgPass[Method](cpg) {

  override def generateParts(): Array[Method] = {
    cpg.method.toArray
  }

  override def runOnPart(diffGraph: DiffGraphBuilder, method: Method): Unit = {
    println(method.name)


    def getPresenceCondition(src: GNode, dst: GNode): String = {
      def calculateStoredNodeId(node: StoredNode): String = "CFG" + node.id().toString

      if (src.nodeKind == 11) {
        src match {
          case node: ControlStructure =>
            val jsonString = src.asInstanceOf[ControlStructure].property[String]("PRESENCE_CONDITION")
            val presenceConditionMap = decode[Map[String, String]](jsonString).getOrElse(Map.empty)

            val dstLocationString = calculateStoredNodeId(dst.asInstanceOf[StoredNode])
            val t = presenceConditionMap.getOrElse(dstLocationString, presenceConditionMap.getOrElse("UNKNOWN", "ERROR"))
            t
          case _ => ""
        }
      }
      else {
        ""
      }
    }


    def presenceConditionBetween(src: GNode, dst: GNode): String = {
      // node we are processing on this path -> (collected presence conditions on this path, visited nodes on this path)
      var workingSet: Map[GNode, (String, Set[GNode])] = Map((src, ("", Set())))
      var presenceConditions: Seq[String] = Seq()

      def expandNode(arg: (GNode, (String, Set[GNode]))): Seq[(GNode, (String, Set[GNode]))] = {
        val (node: GNode, (presenceCondition: String, visitedNodes: Set[GNode])) = arg
        val successorNodes: Seq[GNode] = node.out("CFG").toSeq.filter(!visitedNodes.contains(_))

        successorNodes.map { succNode =>
          val newPresenceCondition = getPresenceCondition(node, succNode) match {
            case "" => presenceCondition
            case value: String if presenceCondition != "" => presenceCondition + " && " + value
            case value: String => value
          }
          if (succNode == dst) {
            presenceConditions = presenceConditions ++ Seq(newPresenceCondition)
            None
          }
          else {
            Some((succNode, (newPresenceCondition, visitedNodes ++ Set(node))))
          }
        }.collect { case Some(v) => v }
      }

      while (workingSet.nonEmpty) {
        workingSet = workingSet.flatMap(expandNode)
      }
      if (presenceConditions.size > 1) {
        presenceConditions.map("(" + _ + ")").mkString(" || ")
      }
      else {
        presenceConditions.mkString
      }
    }


    val reachingDefEdges = method.graph.allEdges.toList.filter(_.property != null)
    val presenceConditions = reachingDefEdges.map { edge =>
      val p1 = presenceConditionBetween(edge.src, edge.dst)
      val p2 = presenceConditionBetween(method, edge.src)
      if (p1 == "") {
        p2
      } else {
        if (p2 == "") {
          p1
        }
        else {
          "(" + p1 + ") && (" + p2 + ")"
        }
      }


    }
    diffGraph.addEdge(method, method._cfgOut.toList.head, "CFG")
    val edgesWithPresenceConditions = reachingDefEdges.zip(presenceConditions).filter((e, p) => p != "")

    edgesWithPresenceConditions.groupBy(_._1.src)
      .foreach { case (src: GNode, edges: List[(Edge, String)]) =>
        val jsonString = src.asInstanceOf[StoredNode].property[String]("PRESENCE_CONDITION")
        val presenceConditionMap = decode[Map[String, String]](jsonString).getOrElse(Map.empty)
        val newPresenceConditionMap: Map[String, String] = presenceConditionMap ++
          edges.map { case (edge, presenceCondition) => "PDG" + edge.dst.id().toString -> presenceCondition }.toMap
        val newPresenceConditionMapSerialized = newPresenceConditionMap.asJson.noSpaces
        diffGraph.setNodeProperty(src, "PRESENCE_CONDITION", newPresenceConditionMapSerialized)
      }
  }

}