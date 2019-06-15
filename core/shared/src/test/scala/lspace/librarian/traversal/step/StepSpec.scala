package lspace.librarian.traversal.step

import java.time.Instant

import lspace.g
import lspace.librarian.traversal.Step
import lspace.provider.detached.DetachedGraph
import lspace.structure.{ClassType, Node}
import monix.eval.Task
import org.scalatest.{AsyncWordSpec, Matchers}
import shapeless.{HList, HNil}

class StepSpec extends AsyncWordSpec with Matchers {
  import lspace.Implicits.Scheduler.global

  def testToNode[S <: Step, S1 <: Step](step: S)(toStep: Node => Task[S1]) =
    (for {
      node    <- step.toNode
      newStep <- toStep(node)
    } yield step shouldBe newStep).runToFuture

  "An And-step" should {
    "be serializable" in {
      testToNode(And(List(g.out(lspace.Label.P.`@createdon`), g.in(lspace.Label.P.`@comment`))))(And.toStep)
    }
  }
  "An As-step" should {
    "be serializable" in {
      testToNode(As("A")(ClassType.stubAny))(As.toStep)
    }
  }
  "A Choose-step" should {
    "be serializable" in {
      testToNode(
        Choose(g.out(lspace.Label.P.`@createdon`), g.in(lspace.Label.P.`@comment`), g.in(lspace.Label.P.`@label`)))(
        Choose.toStep)
    }
  }
  "A Coalesce-step" should {
    "be serializable" in {
      testToNode(Coalesce(
        g.out(lspace.Label.P.`@createdon`) :: g.in(lspace.Label.P.`@comment`) :: g.in(lspace.Label.P.`@label`) :: Nil))(
        Coalesce.toStep)
    }
  }
  "A Coin-step" should {
    "be serializable" in {
      testToNode(Coin(0.5))(Coin.toStep)
    }
  }
  "A Constant-step" should {
    "be serializable" in {
      testToNode(Constant(0.8)(lspace.Label.D.`@double`))(Constant.toStep)
    }
  }
  "A Count-step" should {
    "be serializable" in {
      testToNode(Count: Count)(Count.toStep)
    }
  }
  "A Dedup-step" should {
    "be serializable" in {
      testToNode(Dedup: Dedup)(Dedup.toStep)
    }
  }
  "An E-step" should {
    "be serializable" in {
      testToNode(E(Nil))(E.toStep)
    }
  }
  "A From-step" should {
    "be serializable" in {
      testToNode(From: From)(From.toStep)
    }
  }
  "A G-step" should {
    "be serializable" in {
      testToNode(G(Nil))(G.toStep)
    }
  }
  "A Group-step" should {
    "be serializable" in {
      testToNode(
        Group(lspace.g.out(lspace.Label.P.typed.createdonDateTime),
              lspace.g.out(lspace.Label.P.typed.modifiedonDateTime)))(Group.toStep)
    }
  }
  "A Has-step" should {
    "be serializable" in {
      testToNode(Has(lspace.Label.P.`@label`, Some(lspace.P.eqv("abc"))))(Has.toStep)
    }
  }
  "A HasId-step" should {
    "be serializable" in {
      testToNode(HasId(Set(9999l)))(HasId.toStep)
    }
  }
  "A HasIri-step" should {
    "be serializable" in {
      testToNode(HasIri(Set("example.iri")))(HasIri.toStep)
    }
  }
  "A HasLabel-step" should {
    "be serializable" in {
      testToNode(HasLabel(lspace.Label.P.`@label` :: Nil))(HasLabel.toStep)
    }
  }
  "A Head-step" should {
    "be serializable" in {
      testToNode(Head: Head)(Head.toStep)
    }
  }
  "An Id-step" should {
    "be serializable" in {
      testToNode(Id: Id)(Id.toStep)
    }
  }
  "An In-step" should {
    "be serializable" in {
      testToNode(In(Set(lspace.Label.P.`@label`)))(In.toStep)
    }
  }
  "An InE-step" should {
    "be serializable" in {
      testToNode(InE(Set(lspace.Label.P.`@label`)))(InE.toStep)
    }
  }
  "An InEMap-step" should {
    "be serializable" in {
      testToNode(InEMap(Set(lspace.Label.P.`@label`)))(InEMap.toStep)
    }
  }
  "An InMap-step" should {
    "be serializable" in {
      testToNode(InMap(Set(lspace.Label.P.`@label`)))(InMap.toStep)
    }
  }
  "An Is-step" should {
    "be serializable" in {
      testToNode(Is(lspace.P.eqv("abc")))(Is.toStep)
    }
  }
  "A Label-step" should {
    "be serializable" in {
      testToNode(Label(Set(lspace.Label.P.`@base`)))(Label.toStep)
    }
  }
  "A Last-step" should {
    "be serializable" in {
      testToNode(Last: Last)(Last.toStep)
    }
  }
  "A Limit-step" should {
    "be serializable" in {
      testToNode(Limit(8))(Limit.toStep)
    }
  }
  "A Local-step" should {
    "be serializable" in {
      testToNode(
        Local(lspace.g.out(lspace.Label.P.typed.createdonDateTime))
          .asInstanceOf[Local[ClassType[Any], ClassType[Any]]])(Local.toStep)
    }
  }
  "A Max-step" should {
    "be serializable" in {
      testToNode(Max(lspace.g.out(lspace.Label.P.typed.createdonDateTime)))(Max.toStep)
    }
  }
  "A Mean-step" should {
    "be serializable" in {
      testToNode(Mean: Mean)(Mean.toStep)
    }
  }
  "A Min-step" should {
    "be serializable" in {
      testToNode(Min(lspace.g.out(lspace.Label.P.typed.createdonDateTime)))(Min.toStep)
    }
  }
  "A N-step" should {
    "be serializable" in {
      testToNode(N(DetachedGraph.newNode(9999l) :: Nil))(N.toStep)
    }
  }
  "A Not-step" should {
    "be serializable" in {
      testToNode(Not(lspace.g.out(lspace.Label.P.typed.createdonDateTime)))(Not.toStep)
    }
  }
  "An Or-step" should {
    "be serializable" in {
      testToNode(Or(List(g.out(lspace.Label.P.`@createdon`), g.in(lspace.Label.P.`@comment`))))(Or.toStep)
    }
  }
  "An Order-step" should {
    "be serializable" in {
      testToNode(Order(lspace.g.out(lspace.Label.P.typed.createdonDateTime), true))(Order.toStep)
    }
  }
  "An Out-step" should {
    "be serializable" in {
      testToNode(Out(Set(lspace.Label.P.`@label`)))(Out.toStep)
    }
  }
  "An OutE-step" should {
    "be serializable" in {
      testToNode(OutE(Set(lspace.Label.P.`@label`)))(OutE.toStep)
    }
  }
  "An OutEMap-step" should {
    "be serializable" in {
      testToNode(OutEMap(Set(lspace.Label.P.`@label`)))(OutEMap.toStep)
    }
  }
  "An OutMap-step" should {
    "be serializable" in {
      testToNode(OutMap(Set(lspace.Label.P.`@label`)))(OutMap.toStep)
    }
  }
  "A Path-step" should {
    "be serializable" in {
      testToNode(Path(lspace.g.out(lspace.Label.P.typed.createdonDateTime)).asInstanceOf[Path[ClassType[Any], HList]])(
        Path.toStep)
    }
  }
  "A Project-step" should {
    "be serializable" in {
      testToNode(
        Project(g.out(lspace.Label.P.`@createdon`) :: g
          .in(lspace.Label.P.`@comment`) :: g.in(lspace.Label.P.`@label`) :: HNil)
          .asInstanceOf[Project[HList]])(Project.toStep)
    }
  }
  "A R-step" should {
    "be serializable" in {
      testToNode(R(Nil))(R.toStep)
    }
  }
  "A Range-step" should {
    "be serializable" in {
      testToNode(Range(8, 12))(Range.toStep)
    }
  }
  "A Repeat-step" should {
    "be serializable" in {
      testToNode(
        Repeat(lspace.g.out(lspace.Label.P.typed.createdonDateTime), Some(lspace.g.is(lspace.P.lt(Instant.now))))
          .asInstanceOf[Repeat[ClassType[Any]]])(Repeat.toStep)
    }
  }
  "A Select-step" should {
    "be serializable" in {
      testToNode(Select[Any]("a" :: "b" :: Nil))(Select.toStep)
    }
  }
  "A Sum-step" should {
    "be serializable" in {
      testToNode(Sum: Sum)(Sum.toStep)
    }
  }
  "A Tail-step" should {
    "be serializable" in {
      testToNode(Tail(5))(Tail.toStep)
    }
  }
  "A TimeLimit-step" should {
    "be serializable" in {
      testToNode(TimeLimit(Some(squants.time.Minutes(11))))(TimeLimit.toStep)
    }
  }
  "A To-step" should {
    "be serializable" in {
      testToNode(To: To)(To.toStep)
    }
  }
  "An Union-step" should {
    "be serializable" in {
      testToNode(Union(
        g.out(lspace.Label.P.`@createdon`) :: g.in(lspace.Label.P.`@comment`) :: g.in(lspace.Label.P.`@label`) :: Nil))(
        Union.toStep)
    }
  }
  "A V-step" should {
    "be serializable" in {
      testToNode(V(Nil))(V.toStep)
    }
  }
  "A Where-step" should {
    "be serializable" in {
      testToNode(Where(lspace.g.out(lspace.Label.P.typed.createdonDateTime)))(Where.toStep)
    }
  }
}
