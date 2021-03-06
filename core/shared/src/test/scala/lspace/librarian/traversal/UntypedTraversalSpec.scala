package lspace.librarian.traversal

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class UntypedTraversalSpec extends AnyWordSpec with Matchers {
  val graph = lspace.provider.mem.MemGraph("UntypedTraversalSpec")

//  "The traversal validator" must {
//    "invalidate a traversal which contains segments that contain non-head steps that cannot be moved (idempotent)" in {
//      lspace.g.N
//        .has(Property.default.`@comment`)
//        .out(Property.default.`@id`)
//        .untyped
//        .validate() should matchPattern {
//        case Right(report) =>
//      }
////      lspace.g.N.has(Property.default.`@comment`).count().untyped.validate() should matchPattern {
////        case Left(error) =>
////      }
//    }
//  }

}
