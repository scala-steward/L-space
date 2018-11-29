package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._

object Coin extends StepDef("Coin") with StepWrapper[Coin] {

  def wrap(node: Node): Coin = node match {
    case node: Coin => node
    case _          => Coin(node)
  }

  object keys {
    private val pNode =
      MemGraphDefault.ns.nodes.upsert("sptth/tbd.tld/librarian/step/Coin/p")
    pNode.addLabel(Property.ontology)
    pNode --- Property.default.`@label` --> "p" --- Property.default.`@language` --> "en"
    pNode --- Property.default.`@comment` --> "The p-value thresshold to determine if the traverser keeps on existing" --- Property.default.`@language` --> "en"

    lazy val p                         = Property(pNode)
    val pDouble: TypedProperty[Double] = p + DataType.default.`@double`

    private val seedNode =
      MemGraphDefault.ns.nodes.upsert("sptth/tbd.tld/librarian/step/Coin/seed")
    seedNode.addLabel(Property.ontology)
    seedNode --- Property.default.`@label` --> "seed" --- Property.default.`@language` --> "en"
    seedNode --- Property.default.`@comment` --> "The seed for the random-number generator" --- Property.default.`@language` --> "en"

    lazy val seed                   = Property("sptth/tbd.tld/librarian/step/Coin/seed")
    val seedInt: TypedProperty[Int] = seed + DataType.default.`@int`
  }

  def apply(p: Double, seed: Int = 0): Coin = {
    val node = DetachedGraph.nodes.create(ontology)

    node.addOut(keys.pDouble, p)
    node.addOut(keys.seedInt, seed)
    Coin(node)
  }

  ontologyNode --- Property.default.`@properties` --> keys.p
  ontologyNode --- Property.default.`@properties` --> keys.seed
  //  MemGraphDefault.ns.storeOntology(ontology)
}

case class Coin private (override val value: Node) extends WrappedNode(value) with FilterStep {
  def p: Double =
    out(Coin.keys.pDouble).head //Option.getOrElse(throw new Exception(s"no p-value found??? ${outE(Coin.keys.p).head.to.labels.head.iri} ${out(Coin.keys.p).head.getClass}"))
  def seed: Int                    = out(Coin.keys.seedInt).head //Option.getOrElse(0)
  override def prettyPrint: String = "coin(" + p + ")"
}
