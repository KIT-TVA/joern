package io.joern.c2cpg.passes.variability

import flatgraph.{Edge, GNode}
import io.joern.x2cpg.passes.controlflow.cfgcreation.CfgCreator
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.edges.ReachingDef
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Method}
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.shiftleft.proto.cpg.Cpg.CpgStruct.Node
import io.shiftleft.semanticcpg.language.*
import flatgraph.traversal._
import io.joern.dataflowengineoss.language.ExtendedCfgNode
import io.joern.dataflowengineoss.queryengine.{EngineConfig, EngineContext}
import io.joern.dataflowengineoss.language.toExtendedCfgNode
import io.joern.dataflowengineoss.language.*
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
    Array(arr(0))
  }

  override def runOnPart(diffGraph: DiffGraphBuilder, method: Method): Unit = {
    val bla = method._cfgOut.repeat(_.out("CFG"))()
    println(method.name)
    val localDiff = Cpg.newDiffGraphBuilder
    cpg.astNode
//    reachableBy(bla)


/*
    def findAllPathsBetween(sourceNode: GNode, targetNode: GNode): Vector[List[GNode]] = {
      implicit val context: EngineContext = EngineContext()


      val srcExtended = toExtendedCfgNode(sourceNode)
      val detailedPaths = srcExtended.reachableByDetailed(Iterator.single(targetNode))

      detailedPaths.map(_.path.map(_.node).toList)
    }
*/


    def presenceConditionsReachFromEntry(entry: Method, sources: Iterator[GNode]): Map[GNode, String] = {
      Map()
    }

    def presenceConditionBetween(src: GNode, dst: GNode): String = {
      //TODO: Calculate all possible paths from src to dst and collect presence conditions on the way, then or and return them
      ""
    }

    val reachingDefEdges = method.graph.allEdges.filter(_.property != null)
//    val test = findAllPathsBetween(reachingDefEdges.head.src, reachingDefEdges.head.dst)
    val testEdge = reachingDefEdges.head
    ExtendedCfgNode(testEdge.src.toI) //reachableByFlows(testEdge.dst)


    val reachingDefDefSources = reachingDefEdges.map(_.src)
    val reachingDefSourcesPresenceConditions = presenceConditionsReachFromEntry(method, reachingDefDefSources)

    val reachingDefDestinations = reachingDefDefSources.zip(reachingDefEdges.map(_.dst))

//    val reachingDefEdgePresenceConditions =







    def func(edge: Edge): Unit = {
      //TODO: Wir müssen alle presence conditions sammeln, auf einem pfad verunden und sonst verodern!
      edge.src.asInstanceOf[Call]._astOut
//      bla(edge.dst)
    }

    def collectAllPresenceConditions(src: GNode, dst: GNode, presenceConditions: Option[String] = None): Option[String] = {
      if (src.asInstanceOf[Call]._cfgOut.isEmpty){
        return None
      }
      val paths = src.asInstanceOf[Call]._cfgOut.map{
          node => 
            if(node == dst){
              Some("")
            }
            else{
              val result = collectAllPresenceConditions(node, dst)
            }
        }
      Some("")
    }
//    new CfgCreator(method, localDiff).run()
    diffGraph.absorb(localDiff)
  }

}
