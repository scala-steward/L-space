//package lspace.librarian.traversal.step

//import lspace.provider.detached.DetachedGraph
//import lspace.structure._
//import monix.eval.Task
//
//case object Drop
//    extends StepDef("Drop", "A drop-step removes all resources, held by the traverers, from the graph.")
//    with StepWrapper[Drop]
//    with Drop {
//
//  def toStep(node: Node): Task[Drop] = Task.now(this)
//
//  object keys extends Step.Properties
//  override lazy val properties: List[Property] = Step.properties
//  trait Properties extends Step.Properties
//
//  lazy val toNode: Task[Node]      = DetachedGraph.nodes.create(ontology).memoizeOnSuccess
//  override def prettyPrint: String = "drop()"
//}
//
//trait Drop extends Step
