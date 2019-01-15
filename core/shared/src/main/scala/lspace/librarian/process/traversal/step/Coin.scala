package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.datatype.DataType
import lspace.librarian.structure._

object Coin
    extends StepDef("Coin",
                    "A coin-step flips a coin for each traverser to decide whether it is to live or die.",
                    () => FilterStep.ontology :: Nil)
    with StepWrapper[Coin] {

  def toStep(node: Node): Coin = new Coin(node.out(Coin.keys.pDouble).head, node.out(Coin.keys.seedInt).head)

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

  implicit def toNode(coin: Coin): Node = {
    val node = DetachedGraph.nodes.create(ontology)
    node.addOut(keys.pDouble, coin.p)
    node.addOut(keys.seedInt, coin.seed)
    node
  }

}

case class Coin(p: Double, seed: Int = 0) extends FilterStep {

  def toNode: Node                 = this
  override def prettyPrint: String = "coin(" + p + ")"
}
