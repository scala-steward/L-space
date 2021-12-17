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

  def t: Traversal[ResourceType[Any], ResourceType[Any], (OutKey["name"], OutKey["has:name"], OutKey["has"])] =
    Traversal().out("name").out("has:name").out("has")

  def t2: Traversal[ResourceType[Any], ResourceType[Any], (InKey["name"], OutKey["has:name"], OutKey["has"])] =
    Traversal().in("name").out("has:name").out("has")

  val x: Choose[Traversal[ResourceType[Any], ResourceType[Any], Has["a"] *: EmptyTuple], Traversal[ResourceType[
    Any
  ], ResourceType[
    Any
  ], (Has["b"], Has["c"])], Traversal[ResourceType[Any], ResourceType[Any], OutKey["c"] *: EmptyTuple]] =
    Choose(Traversal().has("a"), Traversal().has("b").has("c"), Traversal().out("c"))

  val x2: Traversal[ResourceType[Any], ResourceType[Any], And[Traversal[
    ResourceType[Any],
    ResourceType[Any],
    Has["a"] *: EmptyTuple
  ] *: Traversal[ResourceType[Any], ResourceType[Any], OutKey["b"] *: EmptyTuple] *: EmptyTuple] *: EmptyTuple] =
    Traversal().and(Traversal().has("a") -> Traversal().out("b"))

  val x3 = Traversal().has("a", P.gt(3))

  val x4: Traversal[ResourceType[Any], StringType.type | IntType.type, 
    Coalesce[(Traversal[ResourceType[Any], StringType.type, HasLabel[StringType.type] *: EmptyTuple], Traversal[ResourceType[Any], IntType.type, HasLabel[IntType.type] *: EmptyTuple])] *: EmptyTuple
  ] = Traversal().coalesce((Traversal().hasLabel[StringType.type], Traversal().hasLabel[IntType.type]))


end TraversalExtensionSpec
