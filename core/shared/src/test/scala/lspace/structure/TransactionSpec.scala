package lspace.structure

import lspace.structure.Property.default._
import lspace.util.SampleGraph
import monix.eval.Task
import org.scalatest.wordspec.AsyncWordSpec
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers

trait TransactionSpec extends AsyncWordSpec with Matchers with BeforeAndAfterAll with GraphFixtures {

  import lspace.Implicits.Scheduler.global
  override def executionContext = lspace.Implicits.Scheduler.global

  def transactionTests(graph: Graph) =
    "a transaction" should {
      "support adding edges to existing nodes" in {
        val t = graph.transaction
        (for {
          node  <- graph.nodes.create(SampleGraph.Person.ontology)
          _     <- node --- `@id` --> "support-transaction-to-add-edges"
          _     <- node --- SampleGraph.Person.keys.name --> "Alice"
          node2 <- t.nodes.hasId(node.id).map(_.get)
          _     <- Task.parSequence(node2.outE(SampleGraph.Person.keys.name).map(_.remove()))
          _     <- node2 --- SampleGraph.Person.keys.balance --> 1.2
          _     <- node2 --- SampleGraph.Person.keys.name --> "Ali"
          _     <- t.commit()
        } yield {
          node.out() should contain(1.2)
          node.out() should not contain ("Alice")
          node.out() should contain("Ali")
          (node.labels should contain).only(SampleGraph.Person.ontology)
        }).runToFuture
      }

      "be merged to the graph only after it is committed" in {
        val newGraph = createGraph("graphspec-support-transactions")
        //      val graphFilled = createSampleGraph("graphspec-support-transactions-filled")
        val transaction = newGraph.transaction

        (for {
          _ <- newGraph.nodes().toListL.map(_.size shouldBe 0)
          _ <- newGraph.edges().toListL.map(_.size shouldBe 0)
          _ <- newGraph.values().toListL.map(_.size shouldBe 0)
          _ <- SampleGraph.loadSocial(transaction)
          _ <- newGraph.nodes().toListL.map(_.size shouldBe 0)
          _ <- newGraph.edges().toListL.map(_.size shouldBe 0)
          _ <- newGraph.values().toListL.map(_.size shouldBe 0)
          _ <- transaction.commit()
          _ <- Task.parZip2(newGraph.nodes.count, graph.nodes.count).map { case (a, b) => a shouldBe b }
          _ <- Task.parZip2(newGraph.edges.count, graph.edges.count).map { case (a, b) => a shouldBe b }
          _ <- Task.parZip2(newGraph.values.count, graph.values.count).map { case (a, b) => a shouldBe b }
        } yield {
          graph.close()
          succeed
        }).runToFuture
      }
    }

}
