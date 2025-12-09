package io.joern.x2cpg.passes.variability

import flatgraph.{Edge, GNode}
import io.joern.x2cpg.passes.controlflow.cfgcreation.CfgCreator
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.edges.ReachingDef
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Method}
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.shiftleft.proto.cpg.Cpg.CpgStruct.Node
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

  override def generateParts(): Array[Method] = cpg.method.toArray

  override def runOnPart(diffGraph: DiffGraphBuilder, method: Method): Unit = {
    val localDiff = Cpg.newDiffGraphBuilder
    cpg.astNode
    val reachingDefEdges = cpg.graph.allEdges.filter(_.property != null)

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
