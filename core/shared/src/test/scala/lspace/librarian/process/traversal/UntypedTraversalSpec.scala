package lspace.librarian.process.traversal

import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.structure.Property
import org.scalatest.{Matchers, WordSpec}

class UntypedTraversalSpec extends WordSpec with Matchers {

  "The traversal validator" must {
    "invalidate a traversal which contains segments that contain non-head steps that cannot be moved (idempotent)" in {
      MemGraphDefault.g.N
        .has(Property.default.`@comment`)
        .out(Property.default.`@id`)
        .untyped
        .validate() should matchPattern {
        case Right(report) =>
      }
      MemGraphDefault.g.N.has(Property.default.`@comment`).count().untyped.validate() should matchPattern {
        case Left(error) =>
      }
    }
  }

}
