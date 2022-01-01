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

  def t: Traversal[ResourceType, ResourceType, Out["name" *: EmptyTuple] *: Out["has:name" *: EmptyTuple] *: Out["has" *: EmptyTuple] *: EmptyTuple] =
    Traversal().out("name".key).out("has:name".key).out("has".key)

  def t2: Traversal[ResourceType, ResourceType, (In["name" *: EmptyTuple], Out["has:name" *: EmptyTuple], Out["has" *: EmptyTuple])] =
    Traversal().in("name".key).out("has:name".key).out("has".key)

  val x: Choose[Traversal[ResourceType, ResourceType, Has["a", Nothing] *: EmptyTuple], Traversal[
    ResourceType,
    ResourceType,
    (Has["b", Nothing], Has["c", Nothing])
  ], Traversal[ResourceType, ResourceType, Out["c" *: EmptyTuple] *: EmptyTuple]] =
    Choose(Traversal().has("a"), Traversal().has("b").has("c"), Traversal().out("c".key))

  val x2: Traversal[ResourceType, ResourceType, And[Traversal[
    ResourceType,
    ResourceType,
    Has["a", Nothing] *: EmptyTuple
  ] *: Traversal[ResourceType, ResourceType, Out["b" *: EmptyTuple] *: EmptyTuple] *: EmptyTuple] *: EmptyTuple] =
    Traversal().and(Traversal().has("a") -> Traversal().out("b".key))

  val x22 = Traversal().and((Traversal().has("a"), Traversal()))

  val x3 = Traversal().has("a", P.gt(3))

  val x4: Traversal[ResourceType, UnionType[String | Int], Coalesce[
    (
      Traversal[ResourceType, StringTyped, HasLabel[StringTyped] *: EmptyTuple],
      Traversal[ResourceType, IntTyped, HasLabel[IntTyped] *: EmptyTuple]
    )
  ] *: EmptyTuple] = Traversal().coalesce((Traversal().hasLabel[StringTyped](StringType), Traversal().hasLabel[IntTyped](IntType)))

  val x44: Traversal[ResourceType, UnionType[String], Coalesce[
    (
      Traversal[ResourceType, StringTyped, HasLabel[StringTyped] *: EmptyTuple],
      Traversal[ResourceType, StringTyped, HasLabel[StringTyped] *: EmptyTuple]
    )
  ] *: EmptyTuple] =
    Traversal().coalesce((Traversal().hasLabel(StringType), Traversal().hasLabel(StringType)))

  Traversal().coalesce((Traversal().hasLabel(StringType), Traversal()))

  val x5 =
    Traversal().choose(Traversal().out("a".key), Traversal().hasLabel(StringType), Traversal().hasLabel(IntType))

  Traversal().constant(3)
  Traversal().constant("a")
// Traversal().constant(0.2)

end TraversalExtensionSpec
