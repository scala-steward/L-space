package lspace.librarian.logic

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.AppendedClues
import org.scalatest.matchers.should.Matchers
import predicate._

trait AssistentSpec extends AnyWordSpec with Matchers with AppendedClues {
  def assistent: Assistent

  import shapeless.=:!=
  def pTests[T, T2, X, P0[Z] <: P[Z]](ah: Assistent#Helper[P[T]],
                                      validValues: List[T2],
                                      invalidValues: List[T2],
                                      incomparableValues: List[X])(implicit /*ev1: T2 <:< T, */ ev2: T =:!= X) =
    s"test ${ah.p.toString} ${ah.p._pvalue.getClass}".which {
      s"is comparable and asserts true to ${validValues.mkString(" and ")}" in {
        validValues.foreach { t =>
          (ah.comparable(t) shouldBe true).withClue(s" => $t should be comparable")
          (ah.assert(t) shouldBe true).withClue(s" => $t")
        }
      }
      s"is comparable and asserts false to ${invalidValues.mkString(" and ")}" in {
        invalidValues.foreach { t =>
          (ah.comparable(t) shouldBe true).withClue(s" => $t should be comparable")
          (ah.assert(t) shouldBe false).withClue(s" => $t")
        }
      }
      s"is not comparable to ${incomparableValues.mkString(" and ")}" in {
        incomparableValues.foreach { x =>
          (ah.comparable(x) shouldBe false).withClue(s" => $x should not be comparable")
        }
      }
    }

  "a string assistent".can {
    pTests(assistent.eqv(Eqv("abc")), "abc" :: Nil, "abcd" :: 1 :: Nil, List[Any]())
    pTests(assistent.neqv(Neqv("abc")), 1 :: "abcd" :: Nil, "abc" :: Nil, List[Any]())
    pTests(assistent.gt(Gt("abc")), "abcd" :: Nil, "abc" :: Nil, 1 :: Nil)
    pTests(assistent.gte(Gte("abc")), "abc" :: Nil, "ab" :: Nil, 1L :: Nil)
    pTests(assistent.lt(Lt("abc")), "ab" :: Nil, "abcd" :: Nil, true :: Nil)
    pTests(assistent.lte(Lte("abc")), "abc" :: Nil, "abcde" :: Nil, new {} :: Nil)
    pTests(assistent.between(Between("ab", "abcdef")), "abc" :: "abcdef" :: Nil, "a" :: Nil, 1 :: Nil)
    pTests(assistent.outside(Outside("ab", "abcdef")), "a" :: "abcdefe" :: Nil, "ab" :: "abc" :: Nil, 1 :: Nil)
    pTests(assistent.inside(Inside("ab", "abcdef")), "abc" :: "abcde" :: Nil, "ab" :: Nil, 1 :: Nil)
  }

  "an numeric assistent".can {
    //Numeric type tests
    pTests(assistent.eqv(Eqv(1)), 1 :: 1.0 :: 1L :: Nil, 2 :: 3L :: 3.3 :: Nil, List[Any]())
    pTests(assistent.eqv(Eqv(2.0)), 2 :: 2.0 :: 2L :: Nil, 1 :: 3L :: 3.3 :: Nil, List[Any]())
    pTests(assistent.eqv(Eqv(3L)), 3 :: 3.0 :: 3L :: Nil, 2 :: 4L :: 3.3 :: Nil, List[Any]())
    pTests(assistent.neqv(Neqv(1)), 2 :: 3L :: 3.3 :: Nil, 1 :: 1.0 :: 1L :: Nil, List[Any]())
    pTests(assistent.lt(Lt(5)), 2 :: 3L :: 3.3 :: Nil, 5 :: 7.0 :: 8L :: Nil, "a" :: new                   {} :: Nil)
    pTests(assistent.lte(Lte(5)), 5 :: 3L :: 3.3 :: Nil, 6 :: 7.0 :: 8L :: Nil, "a" :: new                 {} :: Nil)
    pTests(assistent.gt(Gt(5)), 6 :: 7.0 :: 8L :: Nil, 5 :: 5.0 :: 3L :: 3.3 :: Nil, "a" :: new            {} :: Nil)
    pTests(assistent.gte(Gte(5)), 5 :: 5.0 :: 8L :: Nil, 4 :: 3L :: 3.3 :: Nil, "a" :: new                 {} :: Nil)
    pTests(assistent.between(Between(4, 6)), 5 :: 5.4 :: 6L :: Nil, 11 :: 3L :: 3.3 :: Nil, "a" :: new     {} :: Nil)
    pTests(assistent.between(Between(4.1, 6.0)), 5 :: 5.4 :: 6L :: Nil, 11 :: 3L :: 3.3 :: Nil, "a" :: new {} :: Nil)
    pTests(assistent.between(Between(3.9, 6l)), 5 :: 5.4 :: 6L :: Nil, 11 :: 3L :: 3.3 :: Nil, "a" :: new  {} :: Nil)
    pTests(assistent.outside(Outside(4, 6)), 11 :: 3L :: 3.3 :: Nil, 5 :: 5.4 :: 6L :: Nil, "a" :: new     {} :: Nil)
    pTests(assistent.outside(Outside(4.1, 6.0)), 11 :: 3L :: 3.3 :: Nil, 5 :: 5.4 :: 6L :: Nil, "a" :: new {} :: Nil)
    pTests(assistent.outside(Outside(3.9, 6l)), 11 :: 3L :: 3.3 :: Nil, 5 :: 5.4 :: 6L :: Nil, "a" :: new  {} :: Nil)
    pTests(assistent.inside(Inside(4, 6)), 5 :: 5.4 :: Nil, 11 :: 6 :: 3L :: 3.3 :: Nil, "a" :: new        {} :: Nil)
    pTests(assistent.inside(Inside(4.1, 6.0)), 5 :: 5.4 :: Nil, 11 :: 6 :: 3L :: 3.3 :: Nil, "a" :: new    {} :: Nil)
    pTests(assistent.inside(Inside(3.9, 6l)), 5 :: 5.4 :: Nil, 11 :: 6 :: 3L :: 3.3 :: Nil, "a" :: new     {} :: Nil)
    pTests(assistent.contains(Contains(3)),
           List(3) :: Set(2, 3) :: List(3.0, 4, 5) :: Nil,
           List("a", 2) :: Nil,
           "a" :: 3 :: new {} :: Nil)
    pTests(assistent.contains(Contains(List(2, 3.0))),
           Set(2, 3) :: List(4, 2, 3.0, 5) :: Nil,
           List("a", 2) :: Nil,
           "a" :: 3 :: new {} :: Nil)
    pTests(assistent.disjoint(Disjoint(6)),
           List(3) :: Set(2, 3) :: List(3.0, 4, 5) :: Nil,
           List("a", 6) :: Nil,
           "a" :: 3 :: new {} :: Nil)
    pTests(assistent.disjoint(Disjoint(Set(6, 7))),
           List(3) :: Set(2, 3) :: List(3.0, 4, 5) :: Nil,
           List("a", 6) :: Vector(7, 6) :: Nil,
           "a" :: 3 :: new {} :: Nil)
    pTests(assistent.intersect(Intersect(3)),
           List(3) :: Set("a", 3) :: List(3.0, 4, 5) :: Nil,
           List("a", 6) :: Nil,
           "a" :: 3 :: new {} :: Nil)
    pTests(assistent.intersect(Intersect(Vector(5, 6))),
           List(3.0, 4, 5) :: Nil,
           List(3, 4) :: Nil,
           "a" :: 3 :: new                                                                   {} :: Nil)
    pTests(assistent.within(Within(6)), List(6) :: Nil, List("a", 6) :: Nil, "a" :: 3 :: new {} :: Nil)
    pTests(assistent.within(Within(List(6, 3, 4, 5))),
           List(6) :: Set(4, 3) :: List(3.0, 4, 5) :: Nil,
           List("a", 6) :: Nil,
           "a" :: 3 :: new {} :: Nil)
    pTests(assistent.startsWith(Prefix(6)),
           List(6) :: Vector(6, "a") :: Nil,
           List("a", 6) :: Nil,
           Set(6) :: "a" :: 3 :: new {} :: Nil)
    pTests(assistent.startsWith(Prefix(List(3.0, 4))),
           List(3.0, 4, 5) :: Nil,
           List(6) :: Vector(4, 3) :: List("a", 6) :: Nil,
           "a" :: 3 :: new {} :: Nil)
    pTests(assistent.endsWith(Suffix(6)),
           List("a", 6) :: Nil,
           Vector(6, "a") :: List("5") :: Nil,
           Set(6) :: "a" :: 3 :: new {} :: Nil)
    pTests(assistent.endsWith(Suffix(List(4, 5))),
           List(3.0, 4, 5) :: Nil,
           List(6, "a") :: List(6, "a", 5) :: List(6, "a", 6) :: Nil,
           "a" :: 3 :: new {} :: Nil)
  }

  "a date assistent".can {
    import lspace.datatype.util.Implicits._
    pTests(assistent.eqv(Eqv("1996-08-18".toDate.get)),
           "1996-08-18".toDate.get :: Nil,
           "1996-08-17".toDate.get :: "14:12:00".toTime.get :: 1 :: Nil,
           List[Any]())
    pTests(assistent.neqv(Neqv("1996-08-18".toDate.get)),
           "1996-08-17".toDate.get :: "14:12:00".toTime.get :: Nil,
           "1996-08-18".toDate.get :: Nil,
           List[Any]())
    pTests(assistent.gt(Gt("1996-08-18".toDate.get)),
           "1996-08-19".toDate.get :: Nil,
           "1996-08-17".toDate.get :: Nil,
           "14:12:00".toTime.get :: Nil)
    pTests(assistent.gte(Gte("1996-08-18".toDate.get)),
           "1996-08-18".toDate.get :: Nil,
           "1996-08-16".toDate.get :: Nil,
           "14:12:00".toTime.get :: Nil)
    pTests(
      assistent.lt(Lt("1996-08-18".toDate.get)),
      "1996-08-17".toDate.get :: Nil,
      "1996-08-19".toDate.get :: Nil,
      "14:12:00".toTime.get :: "a" :: Nil
    )
    pTests(
      assistent.lte(Lte("1996-08-18".toDate.get)),
      "1996-08-18".toDate.get :: "1996-08-17".toDate.get :: Nil,
      "1996-08-19".toDate.get :: Nil,
      "14:12:00".toTime.get :: 1 :: Nil
    )
    pTests(
      assistent.between(Between("1996-08-18".toDate.get, "1999-08-18".toDate.get)),
      "1996-08-18".toDate.get :: "1997-08-17".toDate.get :: Nil,
      "1996-04-19".toDate.get :: Nil,
      "14:12:00".toTime.get :: 1 :: Nil
    )
    pTests(
      assistent.outside(Outside("1996-08-18".toDate.get, "1999-08-18".toDate.get)),
      "1996-07-18".toDate.get :: "2004-08-17".toDate.get :: Nil,
      "1996-08-18".toDate.get :: "1996-09-17".toDate.get :: Nil,
      "14:12:00".toTime.get :: 1 :: Nil
    )
    pTests(
      assistent.inside(Inside("1996-08-18".toDate.get, "1999-08-18".toDate.get)),
      "1996-08-19".toDate.get :: "1997-08-17".toDate.get :: Nil,
      "1996-08-18".toDate.get :: "1996-04-19".toDate.get :: Nil,
      "14:12:00".toTime.get :: 1 :: Nil
    )
  }

  "a time assistent".can {
    import lspace.datatype.util.Implicits._
    pTests(assistent.eqv(Eqv("11:13:49".toTime.get)),
           "11:13:49".toTime.get :: Nil,
           "1996-08-17".toDate.get :: "15:15:11".toTime.get :: 1 :: Nil,
           List[Any]())
    pTests(assistent.neqv(Neqv("11:13:49".toTime.get)),
           "1996-08-17".toDate.get :: "14:12:00".toTime.get :: Nil,
           "11:13:49".toTime.get :: Nil,
           List[Any]())
    pTests(assistent.gt(Gt("11:13:49".toTime.get)),
           "11:16:49".toTime.get :: Nil,
           "11:13:49".toTime.get :: "11:11:49".toTime.get :: Nil,
           "1996-08-17".toDate.get :: Nil)
    pTests(assistent.gte(Gte("11:13:49".toTime.get)),
           "11:13:49".toTime.get :: "13:16:49".toTime.get :: Nil,
           "08:13:49".toTime.get :: Nil,
           "1996-08-17".toDate.get :: Nil)
    pTests(
      assistent.lt(Lt("11:13:49".toTime.get)),
      "11:11:49".toTime.get :: Nil,
      "11:16:49".toTime.get :: Nil,
      "1996-08-17".toDate.get :: "a" :: Nil
    )
    pTests(
      assistent.lte(Lte("11:13:49".toTime.get)),
      "11:13:49".toTime.get :: "11:11:49".toTime.get :: Nil,
      "11:16:49".toTime.get :: Nil,
      "1996-08-17".toDate.get :: 1 :: Nil
    )
    pTests(
      assistent.between(Between("11:13:49".toTime.get, "15:13:49".toTime.get)),
      "11:13:49".toTime.get :: "15:11:49".toTime.get :: Nil,
      "11:13:41".toTime.get :: Nil,
      "1996-08-17".toDate.get :: 1 :: Nil
    )
    pTests(
      assistent.outside(Outside("11:13:49".toTime.get, "15:13:49".toTime.get)),
      "10:13:49".toTime.get :: "19:13:49".toTime.get :: Nil,
      "11:13:49".toTime.get :: "12:13:49".toTime.get :: Nil,
      "1996-08-17".toDate.get :: 1 :: Nil
    )
    pTests(
      assistent.inside(Inside("11:13:49".toTime.get, "15:13:49".toTime.get)),
      "11:16:49".toTime.get :: "14:13:49".toTime.get :: Nil,
      "11:13:49".toTime.get :: "16:13:49".toTime.get :: Nil,
      "1996-08-17".toDate.get :: 1 :: Nil
    )
  }

  "a datetime assistent".can {
    import lspace.datatype.util.Implicits._
    pTests(
      assistent.eqv(Eqv("1996-08-18T01:12:00Z".toDateTime.get)),
      "1996-08-18T01:12:00Z".toDateTime.get :: Nil,
      "1996-08-18T01:12:14Z".toDateTime.get :: "14:12:00".toTime.get :: 1 :: Nil,
      List[Any]()
    )
    pTests(
      assistent.neqv(Neqv("1996-08-18T01:12:00Z".toDateTime.get)),
      "1996-08-17T01:12:00Z".toDateTime.get :: "14:12:00".toTime.get :: Nil,
      "1996-08-18T01:12:00Z".toDateTime.get :: Nil,
      List[Any]()
    )
    pTests(
      assistent.gt(Gt("1996-08-18T01:12:00Z".toDateTime.get)),
      "1996-08-19T01:12:00Z".toDateTime.get :: Nil,
      "1996-08-17T01:12:00Z".toDateTime.get :: Nil,
      "14:12:00".toTime.get :: Nil
    )
    pTests(
      assistent.gte(Gte("1996-08-18T01:12:00Z".toDateTime.get)),
      "1996-08-18T01:12:00Z".toDateTime.get :: Nil,
      "1996-08-16T01:12:00Z".toDateTime.get :: Nil,
      "14:12:00".toTime.get :: Nil
    )
    pTests(
      assistent.lt(Lt("1996-08-18T01:12:00Z".toDateTime.get)),
      "1996-08-17T01:12:00Z".toDateTime.get :: Nil,
      "1996-08-19T01:12:00Z".toDateTime.get :: Nil,
      "14:12:00".toTime.get :: "a" :: Nil
    )
    pTests(
      assistent.lte(Lte("1996-08-18T01:12:00Z".toDateTime.get)),
      "1996-08-18T01:12:00Z".toDateTime.get :: "1996-08-17T01:12:00Z".toDateTime.get :: Nil,
      "1996-08-19T01:12:00Z".toDateTime.get :: Nil,
      "14:12:00".toTime.get :: 1 :: Nil
    )
    pTests(
      assistent.between(Between("1996-08-18T01:12:00Z".toDateTime.get, "1999-08-18T01:12:00Z".toDateTime.get)),
      "1996-08-18T01:12:00Z".toDateTime.get :: "1997-08-18T01:12:00Z".toDateTime.get :: Nil,
      "1996-04-18T01:12:00Z".toDateTime.get :: Nil,
      "14:12:00".toTime.get :: 1 :: Nil
    )
    pTests(
      assistent.outside(Outside("1996-08-18T01:12:00Z".toDateTime.get, "1999-08-18T01:12:00Z".toDateTime.get)),
      "1996-08-18T01:11:00Z".toDateTime.get :: "1991-08-18T01:12:00Z".toDateTime.get :: "2004-08-17T01:12:00Z".toDateTime.get :: Nil,
      "1996-08-18T01:12:00Z".toDateTime.get :: "1996-09-17T01:12:00Z".toDateTime.get :: Nil,
      "14:12:00".toTime.get :: 1 :: Nil
    )
    pTests(
      assistent.inside(Inside("1996-08-18T01:12:00Z".toDateTime.get, "1999-08-18T01:12:00Z".toDateTime.get)),
      "1996-08-19T01:12:00Z".toDateTime.get :: "1997-08-17T01:12:00Z".toDateTime.get :: Nil,
      "1996-08-18T01:12:00Z".toDateTime.get :: "1996-04-19T01:12:00Z".toDateTime.get :: Nil,
      "14:12:00".toTime.get :: 1 :: Nil
    )
  }

  "a localdatetime assistent".can {
    import lspace.datatype.util.Implicits._
    pTests(
      assistent.eqv(Eqv("1996-08-18T01:12:00".toLocalDateTime.get)),
      "1996-08-18T01:12:00".toLocalDateTime.get :: Nil,
      "1996-08-18T01:12:14".toLocalDateTime.get :: "14:12:00".toTime.get :: 1 :: Nil,
      List[Any]()
    )
    pTests(
      assistent.neqv(Neqv("1996-08-18T01:12:00".toLocalDateTime.get)),
      "1996-08-17T01:12:00".toLocalDateTime.get :: "14:12:00".toTime.get :: Nil,
      "1996-08-18T01:12:00".toLocalDateTime.get :: Nil,
      List[Any]()
    )
    pTests(
      assistent.gt(Gt("1996-08-18T01:12:00".toLocalDateTime.get)),
      "1996-08-19T01:12:00".toLocalDateTime.get :: Nil,
      "1996-08-17T01:12:00".toLocalDateTime.get :: Nil,
      "14:12:00".toTime.get :: Nil
    )
    pTests(
      assistent.gte(Gte("1996-08-18T01:12:00".toLocalDateTime.get)),
      "1996-08-18T01:12:00".toLocalDateTime.get :: Nil,
      "1996-08-16T01:12:00".toLocalDateTime.get :: Nil,
      "14:12:00".toTime.get :: Nil
    )
    pTests(
      assistent.lt(Lt("1996-08-18T01:12:00".toLocalDateTime.get)),
      "1996-08-17T01:12:00".toLocalDateTime.get :: Nil,
      "1996-08-19T01:12:00".toLocalDateTime.get :: Nil,
      "14:12:00".toTime.get :: "a" :: Nil
    )
    pTests(
      assistent.lte(Lte("1996-08-18T01:12:00".toLocalDateTime.get)),
      "1996-08-18T01:12:00".toLocalDateTime.get :: "1996-08-17T01:12:00".toLocalDateTime.get :: Nil,
      "1996-08-19T01:12:00".toLocalDateTime.get :: Nil,
      "14:12:00".toTime.get :: 1 :: Nil
    )
    pTests(
      assistent.between(Between("1996-08-18T01:12:00".toLocalDateTime.get, "1999-08-18T01:12:00".toLocalDateTime.get)),
      "1996-08-18T01:12:00".toLocalDateTime.get :: "1997-08-18T01:12:00".toLocalDateTime.get :: Nil,
      "1996-04-18T01:12:00".toLocalDateTime.get :: Nil,
      "14:12:00".toTime.get :: 1 :: Nil
    )
    pTests(
      assistent.outside(Outside("1996-08-18T01:12:00".toLocalDateTime.get, "1999-08-18T01:12:00".toLocalDateTime.get)),
      "1996-08-18T01:11:00".toLocalDateTime.get :: "1991-08-18T01:12:00".toLocalDateTime.get :: "2004-08-17T01:12:00".toLocalDateTime.get :: Nil,
      "1996-08-18T01:12:00".toLocalDateTime.get :: "1996-09-17T01:12:00".toLocalDateTime.get :: Nil,
      "14:12:00".toTime.get :: 1 :: Nil
    )
    pTests(
      assistent.inside(Inside("1996-08-18T01:12:00".toLocalDateTime.get, "1999-08-18T01:12:00".toLocalDateTime.get)),
      "1996-08-19T01:12:00".toLocalDateTime.get :: "1997-08-17T01:12:00".toLocalDateTime.get :: Nil,
      "1996-08-18T01:12:00".toLocalDateTime.get :: "1996-04-19T01:12:00".toLocalDateTime.get :: Nil,
      "14:12:00".toTime.get :: 1 :: Nil
    )
  }

  "a geometry assistent".can {
    import lspace.types.geo._
    pTests(
      assistent.eqv(Eqv(Point(4, 9))),
      Point(4, 9) :: Nil,
      Point(4, 4) :: (4, 9) :: 1 :: Nil,
      List[Any]()
    )
    pTests(
      assistent.neqv(Neqv(Point(4, 9))),
      Point(4, 4) :: (4, 9) :: 1 :: Nil,
      Point(4, 9) :: Nil,
      List[Any]()
    )
    pTests(
      assistent.contains(Contains(Point(4, 9))),
      Point(4, 9) :: Polygon(Point(2, 2), Point(12, 2), Point(12, 12), Point(2, 12)) :: Nil,
      Polygon(Point(2, 2), Point(3, 2), Point(3, 3), Point(2, 3)) :: Nil,
      "a" :: 2 :: Nil
    )
    pTests(
      assistent.within(Within(Polygon(Point(2, 2), Point(12, 2), Point(12, 12), Point(2, 12)))),
      Point(4, 9) :: Polygon((3, 3), (11, 3), (11, 11), (3, 11)) :: Nil,
      Point(1, 1) :: Nil,
      "a" :: 2 :: Nil
    )
    pTests(
      assistent.intersect(Intersect(Polygon(Point(2, 2), Point(12, 2), Point(12, 3), Point(2, 3)))),
      Point(4, 2.5) :: Polygon((5, 0), (5, 12), (4, 12), (4, 0)) :: Nil,
      Point(1, 1) :: Nil,
      "a" :: 2 :: Nil
    )
    pTests(
      assistent.disjoint(Disjoint(Polygon(Point(2, 2), Point(12, 2), Point(12, 3), Point(2, 3)))),
      Point(4, 9) :: Polygon((4, 4), (12, 4), (12, 5), (4, 5)) :: Nil,
      Point(5, 2.6) :: Nil,
      "a" :: 2 :: Nil
    )
  }

}
