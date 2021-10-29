package lspace.librarian.traversal.step

import lspace.librarian.traversal._
import lspace.provider.detached.DetachedGraph
import lspace.datatype.DataType
import lspace.structure._
import monix.eval.Task

object Coin
    extends StepDef(
      "Coin",
      "A coin-step flips a coin for each traverser to decide whether it is to live or die.",
      FilterStep.ontology :: Nil
    )
    with StepWrapper[Coin] {

  def toStep(node: Node): Task[Coin] =
    Task.now(new Coin(node.out(Coin.keys.pDouble).head, node.out(Coin.keys.seedInt).head))

  object keys extends FilterStep.Properties {
    object p
        extends PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/step/Coin/p",
          "p",
          "The p-value thresshold to determine if the traverser keeps on existing",
          `@range` = Traversal.ontology :: Nil
        )
    val pDouble: TypedProperty[Double] = p.property.as(DataType.default.`@double`)

    object seed
        extends PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/step/Coin/seed",
          "seed",
          "The seed for the random-number generator",
          `@range` = Traversal.ontology :: Nil
        )
    val seedInt: TypedProperty[Int] = seed.property.as(DataType.default.`@int`)
  }
  override lazy val properties: List[Property] = keys.p.property :: keys.seed.property :: FilterStep.properties

  trait Properties extends FilterStep.Properties {
    val p                              = keys.p
    val pDouble: TypedProperty[Double] = keys.pDouble
    val seed                           = keys.seed
    val seedInt: TypedProperty[Int]    = keys.seedInt
  }

  implicit def toNode(step: Coin): Task[Node] = {
    for {
      node <- DetachedGraph.nodes.create(ontology)
      _    <- node.addOut(keys.pDouble, step.p)
      _    <- node.addOut(keys.seedInt, step.seed)
    } yield node
  }.memoizeOnSuccess

}

case class Coin(p: Double, seed: Int = 0) extends FilterStep {

  lazy val toNode: Task[Node]      = this
  override def prettyPrint: String = "coin(" + p + ")"
}
