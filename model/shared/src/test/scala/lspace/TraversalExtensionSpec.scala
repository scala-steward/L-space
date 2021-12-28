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

  def t: Traversal[ResourceType[Any], ResourceType[Any], Out *: Out *: Out *: EmptyTuple] =
    Traversal().out("name").out("has:name").out("has")

  def t2: Traversal[ResourceType[Any], ResourceType[Any], (In, Out, Out)] =
    Traversal().in("name").out("has:name").out("has")

  val x: Choose[Traversal[ResourceType[Any], ResourceType[Any], Has[Nothing] *: EmptyTuple], Traversal[ResourceType[
    Any
  ], ResourceType[
    Any
  ], (Has[Nothing], Has[Nothing])], Traversal[ResourceType[Any], ResourceType[Any], Out *: EmptyTuple]] =
    Choose(Traversal().has("a"), Traversal().has("b").has("c"), Traversal().out("c"))

  val x2: Traversal[ResourceType[Any], ResourceType[Any], And[Traversal[
    ResourceType[Any],
    ResourceType[Any],
    Has[Nothing] *: EmptyTuple
  ] *: Traversal[ResourceType[Any], ResourceType[Any], Out *: EmptyTuple] *: EmptyTuple] *: EmptyTuple] =
    Traversal().and(Traversal().has("a") -> Traversal().out("b"))

  val x22 = Traversal().and((Traversal().has("a"), Traversal()))

  val x3 = Traversal().has("a", P.gt(3))

  val x4: Traversal[ResourceType[Any], StringType.type | IntType.type, Coalesce[
    (
      Traversal[ResourceType[Any], StringType.type, HasLabel[StringType.type] *: EmptyTuple],
      Traversal[ResourceType[Any], IntType.type, HasLabel[IntType.type] *: EmptyTuple]
    )
  ] *: EmptyTuple] = Traversal().coalesce((Traversal().hasLabel[StringType.type], Traversal().hasLabel[IntType.type]))

  val x44: Traversal[ResourceType[Any], StringType.type, Coalesce[
    (
      Traversal[ResourceType[Any], StringType.type, HasLabel[StringType.type] *: EmptyTuple],
      Traversal[ResourceType[Any], StringType.type, HasLabel[StringType.type] *: EmptyTuple]
    )
  ] *: EmptyTuple] =
    Traversal().coalesce((Traversal().hasLabel[StringType.type], Traversal().hasLabel[StringType.type]))

  Traversal().coalesce((Traversal().hasLabel[StringType.type], Traversal()))

  val x5 =
    Traversal().choose(Traversal().out("a"), Traversal().hasLabel[StringType.type], Traversal().hasLabel[IntType.type])

  Traversal().constant(3)
  Traversal().constant("a")
// Traversal().constant(0.2)

end TraversalExtensionSpec
