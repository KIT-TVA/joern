package io.joern.c2cpg.passes.variability

import flatgraph.GNode
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
    val arr = cpg.method.toArray //TODO: change back!
    arr
  }

  override def runOnPart(diffGraph: DiffGraphBuilder, method: Method): Unit = {
    //    val bla = method._cfgOut.repeat(_.out("CFG"))()
    println(method.name)
    diffGraph.setNodeProperty(method, "LANGUAGE", "HAllooooooooo")
    return
    //    val localDiff = Cpg.newDiffGraphBuilder


    /*    def presenceConditionsReachFromEntry(entry: Method, sources: Iterator[GNode]): Map[GNode, String] = {
          var visitedNodes: Set[GNode] = Set()
          var workingSet: Seq[GNode] = Seq(entry)
          while (workingSet.nonEmpty) {


            visitedNodes = visitedNodes ++ workingSet.toSet
            workingSet = workingSet.filter(visitedNodes.contains(_))
          }
          Map()
        }*/


    def getPresenceCondition(src: GNode, dst: GNode): String = {
      def calculateStoredNodeId(node: StoredNode): String = {
                node.id().toString
       /* val lineNumber = node.properties("LINE_NUMBER").asInstanceOf[Int]
        val columnNumber = node.properties("COLUMN_NUMBER").asInstanceOf[Int]
        val code = node.properties("CODE").asInstanceOf[String]
        s"$lineNumber, $columnNumber $code"*/
      }

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
      var workingSet: Map[GNode, (String, Set[GNode])] = Map((src, ("1", Set())))
      var presenceConditions: Seq[String] = Seq()

      def expandNode(arg: (GNode, (String, Set[GNode]))): Seq[(GNode, (String, Set[GNode]))] = {
        val (node: GNode, (presenceCondition: String, visitedNodes: Set[GNode])) = arg
        val successorNodes: Seq[GNode] = node.out("CFG").toSeq.filter(!visitedNodes.contains(_))

        successorNodes.map { succNode =>
          val newPresenceCondition = getPresenceCondition(node, succNode) match {
            case "" => presenceCondition
            case value: String => presenceCondition + " && " + value
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
      presenceConditions.mkString(" || ")
    }


    val reachingDefEdges = method.graph.allEdges.toList.filter(_.property != null)
    //    val reachingDevSources = reachingDefEdges.map(_.src)
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
          p1 + " && " + p2
        }
      }


    }
    diffGraph.addEdge(method, method._cfgOut.toList.head, "CFG")
    reachingDefEdges.zip(presenceConditions).filter((e, p) => p != "").foreach { (edge, presenceCondition: String) =>
      val jsonString = edge.src.asInstanceOf[StoredNode].property[String]("PRESENCE_CONDITION")
      val presenceConditionMap = decode[Map[String, String]](jsonString).getOrElse(Map.empty)
      val edgeId = "VPDG" + edge.dst.id().toString
      val newPresenceConditionMap = presenceConditionMap + (edgeId -> presenceCondition)
      val newPresenceConditionMapSerialized = newPresenceConditionMap.asJson.noSpaces
      diffGraph.setNodeProperty(edge.src, "PRESENCE_CONDITION", newPresenceConditionMapSerialized)

      println(edge.src.id().toString + " " + newPresenceConditionMapSerialized)
    }


//        presenceConditions.foreach(println)
//    reachingDefEdges.map(_.src).map(_.property("PRESENCE_CONDITION")).foreach(println)
    //    val reachingDefDefSources = reachingDefEdges.map(_.src)
    //    print(test.toList.mkString)
    //    diffGraph.absorb(localDiff)
  }

}
