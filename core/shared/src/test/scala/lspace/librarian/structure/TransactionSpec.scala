package lspace.librarian.structure

import lspace.librarian.util.SampleGraph
import lspace.librarian.structure.Property.default._
import org.scalatest.{AsyncWordSpec, BeforeAndAfterAll, Matchers}

trait TransactionSpec extends AsyncWordSpec with Matchers with BeforeAndAfterAll with GraphFixtures {

  def transactionTests(graph: Graph) = {
    "a transaction" should {
      "support adding edges to existing nodes" in {
        val node = graph.nodes.create(SampleGraph.Person.ontology)
        node --- `@id` --> "support-transaction-to-add-edges"
        node --- SampleGraph.Person.keys.name --> "Alice"
        val t = graph.transaction
        t.nodes.hasId(node.id).foreach { node =>
          node.outE(SampleGraph.Person.keys.name).foreach(_.remove())
          node --- SampleGraph.Person.keys.balance --> 1.2
          node --- SampleGraph.Person.keys.name --> "Ali"
        }
        t.commit()
        node.out() should contain(1.2)
        node.out() should not contain ("Alice")
        node.out() should contain("Ali")
      }

      "be merged to the graph only after it is committed" in {
        val newGraph = createGraph("graphspec-support-transactions")
        //      val graphFilled = createSampleGraph("graphspec-support-transactions-filled")
        val transaction = newGraph.transaction

        newGraph.nodes().size shouldBe 0
        newGraph.edges().size shouldBe 0
        newGraph.values().size shouldBe 0

        SampleGraph.loadSocial(transaction)

        newGraph.nodes().size shouldBe 0
        newGraph.edges().size shouldBe 0
        newGraph.values().size shouldBe 0

        transaction.commit()

        val r = {
          newGraph.nodes().size shouldBe graph.nodes.count
          newGraph.edges().size shouldBe graph.edges.count
          newGraph.values().size shouldBe graph.values.count
        }
        graph.close()
        r
      }
    }
  }

}
