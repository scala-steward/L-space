package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._

object Coin
    extends StepDef("Coin",
                    "A coin-step flips a coin for each traverser to decide whether it is to live or die.",
                    () => FilterStep.ontology :: Nil)
    with StepWrapper[Coin] {

  def wrap(node: Node): Coin = node match {
    case node: Coin => node
    case _          => Coin(node)
  }

  object keys extends FilterStep.Properties {
    object p
        extends Property.PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/step/Coin/p",
          "p",
          "The p-value thresshold to determine if the traverser keeps on existing",
          `@range` = () => Traversal.ontology :: Nil
        )
    val pDouble: TypedProperty[Double] = p.property + DataType.default.`@double`

    object seed
        extends Property.PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/step/Coin/seed",
          "seed",
          "The seed for the random-number generator",
          `@range` = () => Traversal.ontology :: Nil
        )
    val seedInt: TypedProperty[Int] = seed.property + DataType.default.`@int`
  }
  override lazy val properties: List[Property] = keys.p.property :: keys.seed.property :: FilterStep.properties

  trait Properties extends FilterStep.Properties {
    val p                              = keys.p
    val pDouble: TypedProperty[Double] = keys.pDouble
    val seed                           = keys.seed
    val seedInt: TypedProperty[Int]    = keys.seedInt
  }

  def apply(p: Double, seed: Int = 0): Coin = {
    val node = DetachedGraph.nodes.create(ontology)

    node.addOut(keys.pDouble, p)
    node.addOut(keys.seedInt, seed)
    Coin(node)
  }

}

case class Coin private (override val value: Node) extends WrappedNode(value) with FilterStep {
  def p: Double =
    out(Coin.keys.pDouble).head //Option.getOrElse(throw new Exception(s"no p-value found??? ${outE(Coin.keys.p).head.to.labels.head.iri} ${out(Coin.keys.p).head.getClass}"))
  def seed: Int                    = out(Coin.keys.seedInt).head //Option.getOrElse(0)
  override def prettyPrint: String = "coin(" + p + ")"
}
