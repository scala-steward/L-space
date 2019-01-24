package lspace.client.io

import argonaut._
import Argonaut._
import monix.execution.Scheduler
import lspace.librarian.process.computer.{DefaultStreamComputer, GraphComputer}
import lspace.librarian.process.traversal._
import lspace.librarian.provider.mem.MemGraph
import lspace.librarian.structure.Graph
import lspace.librarian.util.SampleGraph
import org.scalatest._
import monix.execution.Scheduler.Implicits.global

import scala.concurrent.ExecutionContext

class LinkedDataServiceSpec extends AsyncWordSpec with Matchers with BeforeAndAfterAll {
  implicit lazy val graph: Graph = MemGraph("DefaultGraphComputerSpec")
  val computer: GraphComputer    = DefaultStreamComputer()
  lazy val g                     = graph.g

  val service: LinkedDataService = LinkedDataServiceImpl(graph)

  implicit override def executionContext: ExecutionContext = global
  //  implicit lazy val scheduler = Scheduler(java.util.concurrent.Executors.newSingleThreadScheduledExecutor(), executionContext)

  override def beforeAll = {
    SampleGraph.loadSocial(graph)
  }

  //  def domapr[Start, End, U >: End, Steps <: HList, RSteps <: HList, Labels <: HList, Containers <: HList](traversal: Traversal[Start, End, Steps, Labels], result: List[Any])(
  //    implicit
  //    reverse: Reverse.Aux[Steps, RSteps],
  //    f: Collect.Aux[RSteps, ContainerSteps.type, Containers],
  //    lf: StructureCalculator[Containers, U]) = service.mapResult[lf.Out, Containers](result, f(traversal.hsteps.steps.reverse))

  "a linked-data service" should {
    "work for a List[String] result" in {
      val traversal     = graph.g.N.has("name", P.eqv("Garrison")).out("name")
      val traversalTask = traversal.toAsyncStream
      traversalTask.runToFuture(global).map { r =>
        assert(r.size == 1)
      }
    }
    "work for a Map[Property, List[Any]] result" in {
      val traversal     = graph.g.N.has("name", P.eqv("Garrison")).outMap()
      val traversalTask = traversal.toAsyncStream
      traversalTask.runToFuture(global).map { r =>
        assert(r.head.size == 5)
      }
    }
    "work for a Map[String, List[Any]] result" in {
      val traversal     = graph.g.N.has("name").group(_.out("name").hasLabel[String]).count
      val traversalTask = traversal.toAsyncStream
      traversalTask.runToFuture(global).map { r =>
        assert(r.size == 1)
      }
    }
  }
}
