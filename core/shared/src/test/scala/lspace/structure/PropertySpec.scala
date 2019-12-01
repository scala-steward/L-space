package lspace.structure

import monix.eval.Task
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AsyncWordSpec

class PropertySpec extends AsyncWordSpec with Matchers {
  import lspace.Implicits.Scheduler.global
  override def executionContext = lspace.Implicits.Scheduler.global

  "Properties" can {
    "be compared by iri" in {
      Task {
        new Property("abc") shouldBe new Property("abc")
        new Property("abc") should not be new Property("abcd")

        val propertyABC = new Property("abc")
        List(propertyABC, propertyABC, propertyABC).toSet.size shouldBe 1

        val propertyABCD = new Property("abcd")
        List(propertyABC, propertyABC, propertyABCD).toSet.size shouldBe 2
      }.runToFuture
    }
  }
  "A property.properties" should {
    ".+ thread-safe" in {
      val p = new Property("a")
      (for {
        _ <- Task.gatherUnordered {
          (1 to 1000).map(i => new Property(s"a$i")).map(p.properties.+(_)).map(Task.now)
        }
      } yield p.properties().size shouldBe 1000).runToFuture
    }
    ".++ thread-safe" in {
      val p = new Property("a")
      (for {
        _ <- Task.gatherUnordered {
          (1 to 1000).map(i => new Property(s"a$i")).grouped(100).map(p.properties.++(_)).map(Task.now).toIterable
        }
      } yield p.properties().size shouldBe 1000).runToFuture
    }
  }
  "A property" can {
    "extends can be circular" in {
      val a: Property = "a"
      val b: Property = "b"
      val c: Property = "c"
      val d: Property = "d"
      a.`@extends`(b) shouldBe false
      a.extendedClasses.+(b)
      a.`@extends`(b) shouldBe true
      b.`@extends`(c) shouldBe false
      b.extendedClasses.+(c)
      b.`@extends`(c) shouldBe true
      a.`@extends`(c) shouldBe true
      a.extendedClasses.+(c)
      a.`@extends`(c) shouldBe true
      b.`@extends`(b) shouldBe false
      b.extendedClasses.+(b)
      b.`@extends`(b) shouldBe true
      a.`@extends`(a) shouldBe false
      c.`@extends`(a) shouldBe false
      c.extendedClasses.+(a)
      c.`@extends`(a) shouldBe true
      a.`@extends`(a) shouldBe true
      a.`@extends`(d) shouldBe false
      b.`@extends`(d) shouldBe false
      a.extendedClasses.+(d)
      a.`@extends`(d) shouldBe true
      b.`@extends`(d) shouldBe true
    }
  }
}
