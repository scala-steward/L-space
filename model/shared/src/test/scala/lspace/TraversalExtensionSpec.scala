package lspace

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import librarian.logic
import librarian.logic.P
import librarian.steps._
import types._

class TraversalExtensionSpec extends AnyWordSpec with Matchers:

  "A traversal".which {
    "starts empty" in {
      """Traversal().has("name".key)""" should compile
      """Traversal().out("name".key)""" should compile
    }
  }

  val t: Traversal[Any, Any, Out["name" *: EmptyTuple] *: Out["has:name" *: EmptyTuple] *: Out[
    "has" *: EmptyTuple
  ] *: EmptyTuple] =
    Traversal().out("name".key).out("has:name".key).out("has".key)

  val t2: Traversal[
    Any,
    Any,
    (In["name" *: EmptyTuple], Out["has:name" *: EmptyTuple], Out["has" *: EmptyTuple])
  ] =
    Traversal().in("name".key).out("has:name".key).out("has".key)

  val x: Choose[Traversal[Any, Any, Has["a", Nothing] *: EmptyTuple], Traversal[
    Any,
    Any,
    (Has["b", Nothing], Has["c", Nothing])
  ], Traversal[Any, Any, Out["c" *: EmptyTuple] *: EmptyTuple]] =
    Choose(Traversal().has("a"), Traversal().has("b").has("c"), Traversal().out("c".key))

  val x2: Traversal[Any, Any, And[Traversal[
    Any,
    Any,
    Has["a", Nothing] *: EmptyTuple
  ] *: Traversal[Any, Any, Out["b" *: EmptyTuple] *: EmptyTuple] *: EmptyTuple] *: EmptyTuple] =
    Traversal().and(Traversal().has("a") -> Traversal().out("b".key))

  val x22 = Traversal().and((Traversal().has("a"), Traversal()))

  val pgt: logic.Gt[Int]              = P.gt(3)
  val pgt3: logic.Gt[3]               = P.gt(3)
  val pnot: logic.Not[logic.Gt[Int]] = P.not(P.gt(3))
  // val pnot3: lspace.Not[Gt[Int]] = P.not(3)

  val x3 = Traversal().has("a", P.gt(3))

  val x4: Traversal[Any, String | Int, Coalesce[
    (
      Traversal[Any, String, HasLabel[String] *: EmptyTuple],
      Traversal[Any, Int, HasLabel[Int] *: EmptyTuple]
    )
  ] *: EmptyTuple] =
    Traversal().coalesce((Traversal().hasLabel[String](), Traversal().hasLabel[Int]()))

  val x44: Traversal[Any, String, Coalesce[
    (
      Traversal[Any, String, HasLabel[String] *: EmptyTuple],
      Traversal[Any, String, HasLabel[String] *: EmptyTuple]
    )
  ] *: EmptyTuple] =
    Traversal().coalesce((Traversal().hasLabel[String](), Traversal().hasLabel[String]()))

  Traversal().coalesce((Traversal().hasLabel[String](), Traversal()))

  val x5 =
    Traversal().choose(Traversal().out("a".key), Traversal().hasLabel[String](), Traversal().hasLabel[Int]())

  Traversal().constant(3)
  Traversal().constant("a")
// Traversal().constant(0.2)

  val x6: Traversal[Any, Int, N *: HasLabel[Int] *: InE["has" *: EmptyTuple] *: To *: EmptyTuple] =
    Traversal().n().hasLabel[Int]().inE("has".key).to()

  val x7: Traversal[Any, String, N *: HasLabel[String] *: OutE["has" *: EmptyTuple] *: From *: EmptyTuple] =
    Traversal().n().hasLabel[String]().outE("has".key).from()

  val union1: Traversal[Any, String | Long, HasLabel[Int] *: Union[Traversal[
    Int,
    String,
    Out["has" *: EmptyTuple] *: HasLabel[String] *: EmptyTuple
  ] *: Traversal[Int, Long, Out["has" *: EmptyTuple] *: HasLabel[Long] *: EmptyTuple] *: EmptyTuple] *: EmptyTuple] =
    Traversal()
      .hasLabel[Int]()
      .union(
        Traversal[Int].out("has".key).hasLabel[String](),
        Traversal[Int].out("has".key).hasLabel[Long]()
      )

  val coalesce1: Traversal[Any, String | Long, HasLabel[Int] *: Coalesce[Traversal[
    Int,
    String,
    Out["has" *: EmptyTuple] *: HasLabel[String] *: EmptyTuple
  ] *: Traversal[Int, Long, Out["has" *: EmptyTuple] *: HasLabel[Long] *: EmptyTuple] *: EmptyTuple] *: EmptyTuple] =
    Traversal()
      .hasLabel[Int]()
      .coalesce(
        Traversal[Int].out("has".key).hasLabel[String](),
        Traversal[Int].out("has".key).hasLabel[Long]()
      )

  // val limit0: Traversal[Any, Any, Limit[0] *: EmptyTuple] = Traversal().limit(0)
  val limit1: Traversal[Any, Any, Limit[1] *: EmptyTuple] = Traversal().limit(1)
  val limit5: Traversal[Any, Any, Limit[5] *: EmptyTuple] = Traversal().limit(5)
  // val limit2: Traversal[Any, Any, Limit[-5] *: EmptyTuple] = Traversal().limit(-5)

  val sum1 = Traversal().hasLabel[Int]().sum()
  val sum2 = Traversal().hasLabel[Double]().sum()

  val repeat1 = Traversal().repeat(Traversal().hasLabel[Int]())
  val repeat2 = Traversal().repeat(Traversal().hasLabel[Int](), Traversal[Int])
// val repeat3 = Traversal().repeat(Traversal().hasLabel[Int](), Traversal[String])
// val repeat4 = Traversal().repeat(Traversal().hasLabel[Int](), Traversal())

  val p0: Probability = 0.0
  val p01: Probability = 0.1
  val p1: Probability = 1.0
  // val p11: Probability = 1.1
  val up1 = Probability.apply(0.1: Double)
  val up2 = Probability.apply(1.1: 1.1)
  val ptest: Probability = (1.1).asInstanceOf[Probability]
  
  val coin1 = Traversal().coin(0.1)
  // val coin2 = Traversal().coin(1.1)

end TraversalExtensionSpec
