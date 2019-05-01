package lspace.structure

import monix.eval.Task
import org.scalatest.{AsyncWordSpec, Matchers}

class PropertySpec extends AsyncWordSpec with Matchers {
  import lspace.Implicits.Scheduler.global

  "Properties" can {
    "be compared by iri" in {
      Task {
        Property("abc") shouldBe Property("abc")
        Property("abc") should not be Property("abcd")

        val propertyABC = Property("abc")
        List(propertyABC, propertyABC, propertyABC).toSet.size shouldBe 1

        val propertyABCD = Property("abcd")
        List(propertyABC, propertyABC, propertyABCD).toSet.size shouldBe 2
      }.runToFuture
    }
  }
  "A property.properties" should {
    ".+ thread-safe" in {
      val p = Property("a")
      (for {
        _ <- Task.gatherUnordered {
          (1 to 1000).map(i => Property(s"a$i")).map(p.properties.+(_)).map(Task.now)
        }
      } yield p.properties().size shouldBe 1000).runToFuture
    }
    ".++ thread-safe" in {
      val p = Property("a")
      (for {
        _ <- Task.gatherUnordered {
          (1 to 1000).map(i => Property(s"a$i")).grouped(100).map(p.properties.++(_)).map(Task.now)
        }
      } yield p.properties().size shouldBe 1000).runToFuture
    }
  }
}
