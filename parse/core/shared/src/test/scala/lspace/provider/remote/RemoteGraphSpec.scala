package lspace.provider.remote

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class RemoteGraphSpec extends AnyWordSpec with Matchers {

  import lspace.Implicits.AsyncGuide.guide
  import lspace.Implicits.Scheduler.global

//  "A remote graph" should {
//    "have a well-parsed uri" in {
//      val graph = RemoteGraph("aaa", "mydomain", 80, "abc/def")
//      graph.serviceUri.toString() shouldBe "mydomain:80/abc/def"
//    }
//  }
}
