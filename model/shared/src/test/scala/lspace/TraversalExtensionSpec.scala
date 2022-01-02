package lspace

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import librarian.step._

class TraversalExtensionSpec extends AnyWordSpec with Matchers:

  "A traversal".which {
    "starts empty" in {
      """Traversal().has("name")""" should compile
      """Traversal().out("name")""" should compile
    }
  }

  def t: Traversal[Any, Any, Out["name" *: EmptyTuple] *: Out["has:name" *: EmptyTuple] *: Out[
    "has" *: EmptyTuple
  ] *: EmptyTuple] =
    Traversal().out("name".key).out("has:name".key).out("has".key)

  def t2: Traversal[
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

  val x3 = Traversal().has("a", P.gt(3))

  val x4: Traversal[Any, String | Int, Coalesce[
    (
      Traversal[Any, String, HasLabel[String] *: EmptyTuple],
      Traversal[Any, Int, HasLabel[Int] *: EmptyTuple]
    )
  ] *: EmptyTuple] =
    Traversal().coalesce((Traversal().hasLabel(StringType.s), Traversal().hasLabel(IntType)))

  val x44: Traversal[Any, String, Coalesce[
    (
      Traversal[Any, String, HasLabel[String] *: EmptyTuple],
      Traversal[Any, String, HasLabel[String] *: EmptyTuple]
    )
  ] *: EmptyTuple] =
    Traversal().coalesce((Traversal().hasLabel(StringType.s), Traversal().hasLabel(StringType.s)))

  Traversal().coalesce((Traversal().hasLabel(StringType.s), Traversal()))

  val x5 =
    Traversal().choose(Traversal().out("a".key), Traversal().hasLabel(StringType.s), Traversal().hasLabel(IntType))

  Traversal().constant(3)
  Traversal().constant("a")
// Traversal().constant(0.2)

  val x6: Traversal[Any, Int, N *: HasLabel[Int] *: InE["has" *: EmptyTuple] *: To *: EmptyTuple] =
    Traversal().n().hasLabel(IntType).inE("has".key).to()

  val x7: Traversal[Any, String, N *: HasLabel[String] *: OutE["has" *: EmptyTuple] *: From *: EmptyTuple] =
    Traversal().n().hasLabel(StringType.s).outE("has".key).from()

  val union1: Traversal[Any, String | Long, HasLabel[Int] *: Union[Traversal[
    Int,
    String,
    Out["has" *: EmptyTuple] *: HasLabel[String] *: EmptyTuple
  ] *: Traversal[Int, Long, Out["has" *: EmptyTuple] *: HasLabel[Long] *: EmptyTuple] *: EmptyTuple] *: EmptyTuple] =
    Traversal()
      .hasLabel(IntType)
      .union(
        Traversal[Int].out("has".key).hasLabel(StringType.s),
        Traversal[Int].out("has".key).hasLabel(LongType.long)
      )

  val coalesce1: Traversal[Any, String | Long, HasLabel[Int] *: Coalesce[Traversal[
    Int,
    String,
    Out["has" *: EmptyTuple] *: HasLabel[String] *: EmptyTuple
  ] *: Traversal[Int, Long, Out["has" *: EmptyTuple] *: HasLabel[Long] *: EmptyTuple] *: EmptyTuple] *: EmptyTuple] =
    Traversal()
      .hasLabel(IntType)
      .coalesce(
        Traversal[Int].out("has".key).hasLabel(StringType.s),
        Traversal[Int].out("has".key).hasLabel(LongType.long)
      )

end TraversalExtensionSpec
