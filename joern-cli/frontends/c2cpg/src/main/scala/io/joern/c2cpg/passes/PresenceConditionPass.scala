package io.joern.c2cpg.passes

import io.joern.c2cpg.Config
import io.joern.c2cpg.astcreation.Defines
import io.joern.x2cpg.passes.frontend.MetaDataPass
import io.joern.x2cpg.{Ast, AstNodeBuilder, ValidationMode}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.{Cpg, EdgeTypes, NodeTypes, PropertyNames}
import io.shiftleft.passes.CpgPass
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal
import superc.core.PresenceConditionManager.PresenceCondition

class PresenceConditionPass(cpg: Cpg, presenceConditionMap: Map[String, PresenceCondition]) extends CpgPass(cpg) {

  private val filename: String                          = "<includes>"
  private val globalName: String                        = NamespaceTraversal.globalNamespaceName
  private val fullName: String                          = MetaDataPass.getGlobalNamespaceBlockFullName(Option(filename))
  private val typeDeclFullNames: Set[String]            = cpg.typeDecl.fullName.toSetImmutable
//  private implicit val schemaValidation: ValidationMode = config.schemaValidation

  override def run(dstGraph: DiffGraphBuilder): Unit = {
    // TODO: Hier dann property hinzufügen?
//      dstGraph.addEdge()
    val controlStructures :List[ControlStructure] = cpg.graph.allNodes.toList.collect {   case cs: ControlStructure => cs }
    val bla = controlStructures.map(_.argumentName)
    controlStructures.foreach{cs =>
      dstGraph.setNodeProperty(cs, PropertyNames.PresenceCondition, presenceConditionMap.get(cs.argumentName.get).toString)}
//    dstGraph.
//    dstGraph.setEdgeProperty(cpg.graph.allEdges.head, "test")
   /* var hadMissingTypeDecl = false
    cpg.typ.filter(typeNeedsTypeDeclStub).foreach { t =>
      val newTypeDecl = NewTypeDecl()
        .name(t.name)
        .fullName(t.typeDeclFullName)
        .code(t.name)
        .isExternal(true)
        .filename(filename)
        .astParentType(NodeTypes.NAMESPACE_BLOCK)
        .astParentFullName(fullName)
      dstGraph.addNode(newTypeDecl)
      createOperatorBinding(newTypeDecl, dstGraph)
      hadMissingTypeDecl = true
    }
    if (hadMissingTypeDecl) Ast.storeInDiffGraph(createGlobalAst(), dstGraph)*/
  }


}
