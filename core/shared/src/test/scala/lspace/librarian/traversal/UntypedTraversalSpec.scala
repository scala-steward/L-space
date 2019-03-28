package lspace.librarian.traversal

import lspace.structure.Property
import org.scalatest.{Matchers, WordSpec}

class UntypedTraversalSpec extends WordSpec with Matchers {
  val graph = lspace.provider.mem.MemGraph("UntypedTraversalSpec")

  "The traversal validator" must {
    "invalidate a traversal which contains segments that contain non-head steps that cannot be moved (idempotent)" in {
      lspace.g.N
        .has(Property.default.`@comment`)
        .out(Property.default.`@id`)
        .untyped
        .validate() should matchPattern {
        case Right(report) =>
      }
      lspace.g.N.has(Property.default.`@comment`).count().untyped.validate() should matchPattern {
        case Left(error) =>
      }
    }
  }

}
