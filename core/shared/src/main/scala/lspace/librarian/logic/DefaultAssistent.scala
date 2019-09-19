package lspace.librarian.logic
import java.time.{Instant, LocalDate, LocalDateTime, LocalTime}

import lspace.librarian.logic.predicate._
import lspace.librarian.util.AssertionNotSupported
import lspace.types.geo._

import scala.collection.immutable.ListSet

object DefaultAssistent {
  def apply(): DefaultAssistent = new DefaultAssistent()
}
class DefaultAssistent extends Assistent {

  object orderhelper extends OrderHelper {
    def int: (Int, Any) => Boolean =
      (v1: Int, v2: Any) =>
        v2 match {
          case v2: Int    => v1 < v2
          case v2: Double => v1 < v2
          case v2: Long   => v1 < v2
          case _          => false
      }
    def double: (Double, Any) => Boolean =
      (v1: Double, v2: Any) =>
        v2 match {
          case v2: Int    => v1 < v2
          case v2: Double => v1 < v2
          case v2: Long   => v1 < v2
          case _          => false
      }
    def long: (Long, Any) => Boolean =
      (v1: Long, v2: Any) =>
        v2 match {
          case v2: Int    => v1 < v2
          case v2: Double => v1 < v2
          case v2: Long   => v1 < v2
          case _          => false
      }
    val datetime: (Instant, Any) => Boolean = (v1: Instant, v2: Any) =>
      v2 match {
        case v2: Instant => v1.isBefore(v2)
        case _           => false
    }
    val localdatetime: (LocalDateTime, Any) => Boolean = (v1: LocalDateTime, v2: Any) =>
      v2 match {
        case v2: LocalDateTime => v1.isBefore(v2)
        case _                 => false
    }
    val localdate: (LocalDate, Any) => Boolean = (v1: LocalDate, v2: Any) =>
      v2 match {
        case v2: LocalDate => v1.isBefore(v2)
        case _             => false
    }
    val localtime: (LocalTime, Any) => Boolean = (v1: LocalTime, v2: Any) =>
      v2 match {
        case v2: LocalTime => v1.isBefore(v2)
        case _             => false
    }
    val string: (String, Any) => Boolean = (v1: String, v2: Any) =>
      v2 match {
        case v2: String => v1 < v2
        case _          => false
    }
  }
  def eqv[T](p: Eqv[T]): Helper[Eqv[T]] = new Helper[Eqv[T]](p) {
    val comparable             = (value: Any) => true
    val assert: Any => Boolean = (value: Any) => value == p.pvalue
  }

  def neqv[T](p: Neqv[T]): Helper[Neqv[T]] = new Helper[Neqv[T]](p) {
    val comparable             = (value: Any) => true
    val assert: Any => Boolean = (value: Any) => value != p.pvalue
  }

  override def gt[T](p: Gt[T]): Helper[Gt[T]] = p.pvalue match {
    case pvalue: String =>
      new Helper[Gt[T]](p) {
        val comparable = (value: Any) => value.isInstanceOf[String]
        val assert: Any => Boolean = {
          case v: String => v > pvalue
        }
      }
    case pvalue: Int =>
      new Helper[Gt[T]](p) {
        val comparable = (value: Any) =>
          value.isInstanceOf[Int] || value.isInstanceOf[Double] || value.isInstanceOf[Long]
        val assert: Any => Boolean = {
          case v: Int    => v > pvalue
          case v: Double => v > pvalue
          case v: Long   => v > pvalue
        }
      }
    case pvalue: Double =>
      new Helper[Gt[T]](p) {
        val comparable = (value: Any) =>
          value.isInstanceOf[Int] || value.isInstanceOf[Double] || value.isInstanceOf[Long]
        val assert: Any => Boolean = {
          case v: Int    => v > pvalue
          case v: Double => v > pvalue
          case v: Long   => v > pvalue
        }
      }
    case pvalue: Long =>
      new Helper[Gt[T]](p) {
        val comparable = (value: Any) =>
          value.isInstanceOf[Int] || value.isInstanceOf[Double] || value.isInstanceOf[Long]
        val assert: Any => Boolean = {
          case v: Int    => v > pvalue
          case v: Double => v > pvalue
          case v: Long   => v > pvalue
        }
      }
    case pvalue: Instant =>
      new Helper[Gt[T]](p) {
        val comparable = (value: Any) => value.isInstanceOf[Instant]
        val assert: Any => Boolean = {
          case v: Instant => v.isAfter(pvalue)
        }
      }
    case pvalue: LocalDateTime =>
      new Helper[Gt[T]](p) {
        val comparable = (value: Any) => value.isInstanceOf[LocalDateTime]
        val assert: Any => Boolean = {
          case v: LocalDateTime => v.isAfter(pvalue)
        }
      }
    case pvalue: LocalDate =>
      new Helper[Gt[T]](p) {
        val comparable = (value: Any) => value.isInstanceOf[LocalDate]
        val assert: Any => Boolean = {
          case v: LocalDate => v.isAfter(pvalue)
        }
      }
    case pvalue: LocalTime =>
      new Helper[Gt[T]](p) {
        val comparable = (value: Any) => value.isInstanceOf[LocalTime]
        val assert: Any => Boolean = {
          case v: LocalTime => v.isAfter(pvalue)
        }
      }
    case pvalue => throw new Exception(s"cannot Gt compare a value of type ${pvalue.getClass.getSimpleName}")
  }

  override def gte[T](p: Gte[T]): Helper[Gte[T]] = p.pvalue match {
    case pvalue: String =>
      new Helper[Gte[T]](p) {
        val comparable = (value: Any) => value.isInstanceOf[String]
        val assert: Any => Boolean = {
          case v: String => v >= pvalue
        }
      }
    case pvalue: Int =>
      new Helper[Gte[T]](p) {
        val comparable = (value: Any) =>
          value.isInstanceOf[Int] || value.isInstanceOf[Double] || value.isInstanceOf[Long]
        val assert: Any => Boolean = {
          case v: Int    => v >= pvalue
          case v: Double => v >= pvalue
          case v: Long   => v >= pvalue
        }
      }
    case pvalue: Double =>
      new Helper[Gte[T]](p) {
        val comparable = (value: Any) =>
          value.isInstanceOf[Int] || value.isInstanceOf[Double] || value.isInstanceOf[Long]
        val assert: Any => Boolean = {
          case v: Int    => v >= pvalue
          case v: Double => v >= pvalue
          case v: Long   => v >= pvalue
        }
      }
    case pvalue: Long =>
      new Helper[Gte[T]](p) {
        val comparable = (value: Any) =>
          value.isInstanceOf[Int] || value.isInstanceOf[Double] || value.isInstanceOf[Long]
        val assert: Any => Boolean = {
          case v: Int    => v >= pvalue
          case v: Double => v >= pvalue
          case v: Long   => v >= pvalue
        }
      }
    case pvalue: Instant =>
      new Helper[Gte[T]](p) {
        val comparable = (value: Any) => value.isInstanceOf[Instant]
        val assert: Any => Boolean = {
          case v: Instant => v == pvalue || v.isAfter(pvalue)
        }
      }
    case pvalue: LocalDateTime =>
      new Helper[Gte[T]](p) {
        val comparable = (value: Any) => value.isInstanceOf[LocalDateTime]
        val assert: Any => Boolean = {
          case v: LocalDateTime => v == pvalue || v.isAfter(pvalue)
        }
      }
    case pvalue: LocalDate =>
      new Helper[Gte[T]](p) {
        val comparable = (value: Any) => value.isInstanceOf[LocalDate]
        val assert: Any => Boolean = {
          case v: LocalDate => v == pvalue || v.isAfter(pvalue)
        }
      }
    case pvalue: LocalTime =>
      new Helper[Gte[T]](p) {
        val comparable = (value: Any) => value.isInstanceOf[LocalTime]
        val assert: Any => Boolean = {
          case v: LocalTime => v == pvalue || v.isAfter(pvalue)
        }
      }
    case pvalue => throw new Exception(s"cannot Gte compare a value of type ${pvalue.getClass.getSimpleName}")
  }

  override def lt[T](p: Lt[T]): Helper[Lt[T]] = p.pvalue match {
    case pvalue: String =>
      new Helper[Lt[T]](p) {
        val comparable = (value: Any) => value.isInstanceOf[String]
        val assert: Any => Boolean = {
          case v: String => v < pvalue
        }
      }
    case pvalue: Int =>
      new Helper[Lt[T]](p) {
        val comparable = (value: Any) =>
          value.isInstanceOf[Int] || value.isInstanceOf[Double] || value.isInstanceOf[Long]
        val assert: Any => Boolean = {
          case v: Int    => v < pvalue
          case v: Double => v < pvalue
          case v: Long   => v < pvalue
        }
      }
    case pvalue: Double =>
      new Helper[Lt[T]](p) {
        val comparable = (value: Any) =>
          value.isInstanceOf[Int] || value.isInstanceOf[Double] || value.isInstanceOf[Long]
        val assert: Any => Boolean = {
          case v: Int    => v < pvalue
          case v: Double => v < pvalue
          case v: Long   => v < pvalue
        }
      }
    case pvalue: Long =>
      new Helper[Lt[T]](p) {
        val comparable = (value: Any) =>
          value.isInstanceOf[Int] || value.isInstanceOf[Double] || value.isInstanceOf[Long]
        val assert: Any => Boolean = {
          case v: Int    => v < pvalue
          case v: Double => v < pvalue
          case v: Long   => v < pvalue
        }
      }
    case pvalue: Instant =>
      new Helper[Lt[T]](p) {
        val comparable = (value: Any) => value.isInstanceOf[Instant]
        val assert: Any => Boolean = {
          case v: Instant => v.isBefore(pvalue)
        }
      }
    case pvalue: LocalDateTime =>
      new Helper[Lt[T]](p) {
        val comparable = (value: Any) => value.isInstanceOf[LocalDateTime]
        val assert: Any => Boolean = {
          case v: LocalDateTime => v.isBefore(pvalue)
        }
      }
    case pvalue: LocalDate =>
      new Helper[Lt[T]](p) {
        val comparable = (value: Any) => value.isInstanceOf[LocalDate]
        val assert: Any => Boolean = {
          case v: LocalDate => v.isBefore(pvalue)
        }
      }
    case pvalue: LocalTime =>
      new Helper[Lt[T]](p) {
        val comparable = (value: Any) => value.isInstanceOf[LocalTime]
        val assert: Any => Boolean = {
          case v: LocalTime => v.isBefore(pvalue)
        }
      }
    case pvalue => throw new Exception(s"cannot Lt compare a value of type ${pvalue.getClass.getSimpleName}")
  }

  override def lte[T](p: Lte[T]): Helper[Lte[T]] = p.pvalue match {
    case pvalue: String =>
      new Helper[Lte[T]](p) {
        val comparable = (value: Any) => value.isInstanceOf[String]
        val assert: Any => Boolean = {
          case v: String => v <= pvalue
        }
      }
    case pvalue: Int =>
      new Helper[Lte[T]](p) {
        val comparable = (value: Any) =>
          value.isInstanceOf[Int] || value.isInstanceOf[Double] || value.isInstanceOf[Long]
        val assert: Any => Boolean = {
          case v: Int    => v <= pvalue
          case v: Double => v <= pvalue
          case v: Long   => v <= pvalue
        }
      }
    case pvalue: Double =>
      new Helper[Lte[T]](p) {
        val comparable = (value: Any) =>
          value.isInstanceOf[Int] || value.isInstanceOf[Double] || value.isInstanceOf[Long]
        val assert: Any => Boolean = {
          case v: Int    => v <= pvalue
          case v: Double => v <= pvalue
          case v: Long   => v <= pvalue
        }
      }
    case pvalue: Long =>
      new Helper[Lte[T]](p) {
        val comparable = (value: Any) =>
          value.isInstanceOf[Int] || value.isInstanceOf[Double] || value.isInstanceOf[Long]
        val assert: Any => Boolean = {
          case v: Int    => v <= pvalue
          case v: Double => v <= pvalue
          case v: Long   => v <= pvalue
        }
      }
    case pvalue: Instant =>
      new Helper[Lte[T]](p) {
        val comparable = (value: Any) => value.isInstanceOf[Instant]
        val assert: Any => Boolean = {
          case v: Instant => v == pvalue || v.isBefore(pvalue)
        }
      }
    case pvalue: LocalDateTime =>
      new Helper[Lte[T]](p) {
        val comparable = (value: Any) => value.isInstanceOf[LocalDateTime]
        val assert: Any => Boolean = {
          case v: LocalDateTime => v == pvalue || v.isBefore(pvalue)
        }
      }
    case pvalue: LocalDate =>
      new Helper[Lte[T]](p) {
        val comparable = (value: Any) => value.isInstanceOf[LocalDate]
        val assert: Any => Boolean = {
          case v: LocalDate => v == pvalue || v.isBefore(pvalue)
        }
      }
    case pvalue: LocalTime =>
      new Helper[Lte[T]](p) {
        val comparable = (value: Any) => value.isInstanceOf[LocalTime]
        val assert: Any => Boolean = {
          case v: LocalTime => v == pvalue || v.isBefore(pvalue)
        }
      }
    case pvalue => throw new Exception(s"cannot Lte compare a value of type ${pvalue.getClass.getSimpleName}")
  }

  override def between[T](p: Between[T]): Helper[Between[T]] = (p.lower, p.upper) match {
    case (lower: String, upper: String) =>
      new Helper[Between[T]](p) {
        val comparable = (value: Any) => value.isInstanceOf[String]
        val assert: Any => Boolean = {
          case v: String => lower <= v && v <= upper
        }
      }
    case (lower: Instant, upper: Instant) =>
      new Helper[Between[T]](p) {
        val comparable = (value: Any) => value.isInstanceOf[Instant]
        val assert: Any => Boolean = {
          case v: Instant => (v == lower || v.isAfter(lower)) && (v == upper || v.isBefore(upper))
        }
      }
    case (lower: LocalDateTime, upper: LocalDateTime) =>
      new Helper[Between[T]](p) {
        val comparable = (value: Any) => value.isInstanceOf[LocalDateTime]
        val assert: Any => Boolean = {
          case v: LocalDateTime => (v == lower || v.isAfter(lower)) && (v == upper || v.isBefore(upper))
        }
      }
    case (lower: LocalDate, upper: LocalDate) =>
      new Helper[Between[T]](p) {
        val comparable = (value: Any) => value.isInstanceOf[LocalDate]
        val assert: Any => Boolean = {
          case v: LocalDate => (v == lower || v.isAfter(lower)) && (v == upper || v.isBefore(upper))
        }
      }
    case (lower: LocalTime, upper: LocalTime) =>
      new Helper[Between[T]](p) {
        val comparable = (value: Any) => value.isInstanceOf[LocalTime]
        val assert: Any => Boolean = {
          case v: LocalTime => (v == lower || v.isAfter(lower)) && (v == upper || v.isBefore(upper))
        }
      }
    case (lower, upper) =>
      val (lbTest, ubTest) =
        (lower match {
          case lower: Int =>
            (v: Any) =>
              v match {
                case v: Int    => lower <= v
                case v: Double => lower <= v
                case v: Long   => lower <= v
              }
          case lower: Double =>
            (v: Any) =>
              v match {
                case v: Int    => lower <= v
                case v: Double => lower <= v
                case v: Long   => lower <= v
              }
          case lower: Long =>
            (v: Any) =>
              v match {
                case v: Int    => lower <= v
                case v: Double => lower <= v
                case v: Long   => lower <= v
              }
          case lower => throw new Exception(s"cannot Between compare a value of type ${lower.getClass.getSimpleName}")
        }) ->
          (upper match {
            case upper: Int =>
              (v: Any) =>
                v match {
                  case v: Int    => v <= upper
                  case v: Double => v <= upper
                  case v: Long   => v <= upper
                }
            case upper: Double =>
              (v: Any) =>
                v match {
                  case v: Int    => v <= upper
                  case v: Double => v <= upper
                  case v: Long   => v <= upper
                }
            case upper: Long =>
              (v: Any) =>
                v match {
                  case v: Int    => v <= upper
                  case v: Double => v <= upper
                  case v: Long   => v <= upper
                }
            case upper => throw new Exception(s"cannot Between compare a value of type ${upper.getClass.getSimpleName}")
          })
      new Helper[Between[T]](p) {
        val comparable = (value: Any) =>
          value.isInstanceOf[Int] || value.isInstanceOf[Double] || value.isInstanceOf[Long]
        val assert: Any => Boolean = (value: Any) => lbTest(value) && ubTest(value)
      }
  }

  override def outside[T](p: Outside[T]): Helper[Outside[T]] = (p.lower, p.upper) match {
    case (lower: String, upper: String) =>
      new Helper[Outside[T]](p) {
        val comparable = (value: Any) => value.isInstanceOf[String]
        val assert: Any => Boolean = {
          case v: String => lower > v || v > upper
        }
      }
    case (lower: Instant, upper: Instant) =>
      new Helper[Outside[T]](p) {
        val comparable = (value: Any) => value.isInstanceOf[Instant]
        val assert: Any => Boolean = {
          case v: Instant => v.isBefore(lower) || v.isAfter(upper)
        }
      }
    case (lower: LocalDateTime, upper: LocalDateTime) =>
      new Helper[Outside[T]](p) {
        val comparable = (value: Any) => value.isInstanceOf[LocalDateTime]
        val assert: Any => Boolean = {
          case v: LocalDateTime => v.isBefore(lower) || v.isAfter(upper)
        }
      }
    case (lower: LocalDate, upper: LocalDate) =>
      new Helper[Outside[T]](p) {
        val comparable = (value: Any) => value.isInstanceOf[LocalDate]
        val assert: Any => Boolean = {
          case v: LocalDate => v.isBefore(lower) || v.isAfter(upper)
        }
      }
    case (lower: LocalTime, upper: LocalTime) =>
      new Helper[Outside[T]](p) {
        val comparable = (value: Any) => value.isInstanceOf[LocalTime]
        val assert: Any => Boolean = {
          case v: LocalTime => v.isBefore(lower) || v.isAfter(upper)
        }
      }
    case (lower, upper) =>
      val (lbTest, ubTest) =
        (lower match {
          case lower: Int =>
            (v: Any) =>
              v match {
                case v: Int    => lower > v
                case v: Double => lower > v
                case v: Long   => lower > v
              }
          case lower: Double =>
            (v: Any) =>
              v match {
                case v: Int    => lower > v
                case v: Double => lower > v
                case v: Long   => lower > v
              }
          case lower: Long =>
            (v: Any) =>
              v match {
                case v: Int    => lower > v
                case v: Double => lower > v
                case v: Long   => lower > v
              }
          case lower => throw new Exception(s"cannot Outside compare a value of type ${lower.getClass.getSimpleName}")
        }) ->
          (upper match {
            case upper: Int =>
              (v: Any) =>
                v match {
                  case v: Int    => v > upper
                  case v: Double => v > upper
                  case v: Long   => v > upper
                }
            case upper: Double =>
              (v: Any) =>
                v match {
                  case v: Int    => v > upper
                  case v: Double => v > upper
                  case v: Long   => v > upper
                }
            case upper: Long =>
              (v: Any) =>
                v match {
                  case v: Int    => v > upper
                  case v: Double => v > upper
                  case v: Long   => v > upper
                }
            case upper => throw new Exception(s"cannot Outside compare a value of type ${upper.getClass.getSimpleName}")
          })
      new Helper[Outside[T]](p) {
        val comparable = (value: Any) =>
          value.isInstanceOf[Int] || value.isInstanceOf[Double] || value.isInstanceOf[Long]
        val assert: Any => Boolean = (value: Any) => lbTest(value) || ubTest(value)
      }
  }

  override def inside[T](p: Inside[T]): Helper[Inside[T]] = (p.lower, p.upper) match {
    case (lower: String, upper: String) =>
      new Helper[Inside[T]](p) {
        val comparable = (value: Any) => value.isInstanceOf[String]
        val assert: Any => Boolean = {
          case v: String => lower < v && v < upper
        }
      }
    case (lower: Instant, upper: Instant) =>
      new Helper[Inside[T]](p) {
        val comparable = (value: Any) => value.isInstanceOf[Instant]
        val assert: Any => Boolean = {
          case v: Instant => v.isAfter(lower) && v.isBefore(upper)
        }
      }
    case (lower: LocalDateTime, upper: LocalDateTime) =>
      new Helper[Inside[T]](p) {
        val comparable = (value: Any) => value.isInstanceOf[LocalDateTime]
        val assert: Any => Boolean = {
          case v: LocalDateTime => v.isAfter(lower) && v.isBefore(upper)
        }
      }
    case (lower: LocalDate, upper: LocalDate) =>
      new Helper[Inside[T]](p) {
        val comparable = (value: Any) => value.isInstanceOf[LocalDate]
        val assert: Any => Boolean = {
          case v: LocalDate => v.isAfter(lower) && v.isBefore(upper)
        }
      }
    case (lower: LocalTime, upper: LocalTime) =>
      new Helper[Inside[T]](p) {
        val comparable = (value: Any) => value.isInstanceOf[LocalTime]
        val assert: Any => Boolean = {
          case v: LocalTime => v.isAfter(lower) && v.isBefore(upper)
        }
      }
    case (lower, upper) =>
      val (lbTest, ubTest) =
        (lower match {
          case lower: Int =>
            (v: Any) =>
              v match {
                case v: Int    => lower < v
                case v: Double => lower < v
                case v: Long   => lower < v
              }
          case lower: Double =>
            (v: Any) =>
              v match {
                case v: Int    => lower < v
                case v: Double => lower < v
                case v: Long   => lower < v
              }
          case lower: Long =>
            (v: Any) =>
              v match {
                case v: Int    => lower < v
                case v: Double => lower < v
                case v: Long   => lower < v
              }
          case lower => throw new Exception(s"cannot Inside compare a value of type ${lower.getClass.getSimpleName}")
        }) ->
          (upper match {
            case upper: Int =>
              (v: Any) =>
                v match {
                  case v: Int    => v < upper
                  case v: Double => v < upper
                  case v: Long   => v < upper
                }
            case upper: Double =>
              (v: Any) =>
                v match {
                  case v: Int    => v < upper
                  case v: Double => v < upper
                  case v: Long   => v < upper
                }
            case upper: Long =>
              (v: Any) =>
                v match {
                  case v: Int    => v < upper
                  case v: Double => v < upper
                  case v: Long   => v < upper
                }
            case upper => throw new Exception(s"cannot Inside compare a value of type ${upper.getClass.getSimpleName}")
          })
      new Helper[Inside[T]](p) {
        val comparable = (value: Any) =>
          value.isInstanceOf[Int] || value.isInstanceOf[Double] || value.isInstanceOf[Long]
        val assert: Any => Boolean = (value: Any) => lbTest(value) && ubTest(value)
      }
  }

  private def equalsToList[T](t: T): List[Any] = t match {
    case v: ListSet[_]   => v.toList
    case v: List[_]      => v
    case v: Set[_]       => v.toList
    case v: Vector[_]    => v.toList
    case v: (_, _)       => v.productIterator.toList
    case v: (_, _, _)    => v.productIterator.toList
    case v: (_, _, _, _) => v.productIterator.toList
    case v               => List(v)
  }

  def intersectHelper[T](p: Intersect[T]) = new Helper[Intersect[T]](p) {
    val pvalue     = p.pvalue
    val pvalueList = equalsToList(pvalue)
    val comparable = (value: Any) =>
      value.isInstanceOf[Iterable[_]] || value.isInstanceOf[(_, _)] || value.isInstanceOf[(_, _, _)] || value
        .isInstanceOf[(_, _, _, _)] //nP contains comparable?
    val assert: Any => Boolean = {
      case v: ListSet[_]   => pvalueList.intersect(v.toList).nonEmpty
      case v: List[_]      => pvalueList.intersect(v).nonEmpty
      case v: Set[_]       => pvalueList.intersect(v.toList).nonEmpty
      case v: Vector[_]    => pvalueList.intersect(v.toList).nonEmpty
      case v: (_, _)       => pvalueList.intersect(v.productIterator.toList).nonEmpty
      case v: (_, _, _)    => pvalueList.intersect(v.productIterator.toList).nonEmpty
      case v: (_, _, _, _) => pvalueList.intersect(v.productIterator.toList).nonEmpty
      case v               => false
    }
  }
  def intersectStringHelper[T <: String](p: Intersect[T]) = new Helper[Intersect[T]](p) {
    val pvalue     = p.pvalue
    val comparable = (value: Any) => value.isInstanceOf[String] //???
    val assert: Any => Boolean = {
      case v: String => pvalue.intersect(v).nonEmpty
    }
  }
  def intersectGeoHelper[T <: Geometry](p: Intersect[T]) = new Helper[Intersect[T]](p) {
    val pvalue     = p.pvalue
    val comparable = (value: Any) => value.isInstanceOf[Geometry] //???
    val assert: Any => Boolean = {
      case v: Geometry => v.intersect(pvalue)
    }
  }

  override def intersect[T](p: Intersect[T]): Helper[Intersect[T]] =
    (p.pvalue match {
      case v: String   => intersectStringHelper(p.asInstanceOf[Intersect[String]])
      case v: Geometry => intersectGeoHelper(p.asInstanceOf[Intersect[Geometry]])
      case v           => intersectHelper(p)
    }).asInstanceOf[Helper[Intersect[T]]]

  def containsHelper[T](p: Contains[T]) = new Helper[Contains[T]](p) {
    val pvalue     = p.pvalue
    val pvalueList = equalsToList(pvalue)
    //TODO: find nested P's and create corresponding helper
    val comparable = (value: Any) =>
      value.isInstanceOf[Iterable[_]] || value.isInstanceOf[(_, _)] || value.isInstanceOf[(_, _, _)] || value
        .isInstanceOf[(_, _, _, _)] //nP contains comparable?
    val assert: Any => Boolean = {
      case v: ListSet[_]   => v.toList.containsSlice(pvalueList)
      case v: List[_]      => v.containsSlice(pvalueList)
      case v: Set[_]       => pvalueList.intersect(v.toList).size == pvalueList.size
      case v: Vector[_]    => v.toList.containsSlice(pvalueList)
      case v: (_, _)       => v.productIterator.toList.containsSlice(pvalueList)
      case v: (_, _, _)    => v.productIterator.toList.containsSlice(pvalueList)
      case v: (_, _, _, _) => v.productIterator.toList.containsSlice(pvalueList)
      case v               => false
    }
  }
  def containsStringHelper[T <: String](p: Contains[T]) = new Helper[Contains[T]](p) {
    val pvalue     = p.pvalue
    val comparable = (value: Any) => value.isInstanceOf[String] //???
    val assert: Any => Boolean = {
      case v: String => v.contains(pvalue)
    }
  }
  def containsGeoHelper[T <: Geometry](p: Contains[T]) = new Helper[Contains[T]](p) {
    val pvalue     = p.pvalue
    val comparable = (value: Any) => value.isInstanceOf[Geometry] //???
    val assert: Any => Boolean = {
      case v: Geometry => v.contains(pvalue)
    }
  }
  def containsPHelper[T <: P[_]](p: Contains[T]) = new Helper[Contains[T]](p) {
    val pvalue = p.pvalue
    val comparable = (value: Any) =>
      value.isInstanceOf[Iterable[_]] || value.isInstanceOf[(_, _)] || value.isInstanceOf[(_, _, _)] || value
        .isInstanceOf[(_, _, _, _)] //nP contains comparable?
    val nP = pToHelper(pvalue)
    val assert: Any => Boolean = {
      case v: ListSet[_]   => v.exists(nP.assert)
      case v: List[_]      => v.exists(nP.assert)
      case v: Set[_]       => v.exists(nP.assert)
      case v: Vector[_]    => v.exists(nP.assert)
      case v: (_, _)       => v.productIterator.toList.exists(nP.assert)
      case v: (_, _, _)    => v.productIterator.toList.exists(nP.assert)
      case v: (_, _, _, _) => v.productIterator.toList.exists(nP.assert)
    }
  }
  override def contains[T](p: Contains[T]): Helper[Contains[T]] =
    (p.pvalue match {
      case v: P[_]     => containsPHelper(p.asInstanceOf[Contains[P[_]]])
      case v: String   => containsStringHelper(p.asInstanceOf[Contains[String]])
      case v: Geometry => containsGeoHelper(p.asInstanceOf[Contains[Geometry]])
      case v           => containsHelper(p)
    }).asInstanceOf[Helper[Contains[T]]]

  def disjointHelper[T](p: Disjoint[T]) = new Helper[Disjoint[T]](p) {
    val pvalue     = p.pvalue
    val pvalueList = equalsToList(pvalue)
    val comparable = (value: Any) =>
      value.isInstanceOf[Iterable[_]] || value.isInstanceOf[(_, _)] || value.isInstanceOf[(_, _, _)] || value
        .isInstanceOf[(_, _, _, _)] //nP contains comparable?
    val assert: Any => Boolean = {
      case v: ListSet[_]   => pvalueList.intersect(v.toList).isEmpty
      case v: List[_]      => pvalueList.intersect(v).isEmpty
      case v: Set[_]       => pvalueList.intersect(v.toList).isEmpty
      case v: Vector[_]    => pvalueList.intersect(v.toList).isEmpty
      case v: (_, _)       => pvalueList.intersect(v.productIterator.toList).isEmpty
      case v: (_, _, _)    => pvalueList.intersect(v.productIterator.toList).isEmpty
      case v: (_, _, _, _) => pvalueList.intersect(v.productIterator.toList).isEmpty
      case v               => true
    }
  }
  def disjointStringHelper[T <: String](p: Disjoint[T]) = new Helper[Disjoint[T]](p) {
    val pvalue     = p.pvalue
    val comparable = (value: Any) => value.isInstanceOf[String] //???
    val assert: Any => Boolean = {
      case v: String => pvalue.intersect(v).isEmpty
    }
  }
  def disjointGeoHelper[T <: Geometry](p: Disjoint[T]) = new Helper[Disjoint[T]](p) {
    val pvalue     = p.pvalue
    val comparable = (value: Any) => value.isInstanceOf[Geometry] //???
    val assert: Any => Boolean = {
      case v: Geometry => v.disjoint(pvalue)
    }
  }
  override def disjoint[T](p: Disjoint[T]): Helper[Disjoint[T]] =
    (p.pvalue match {
      case v: String   => disjointStringHelper(p.asInstanceOf[Disjoint[String]])
      case v: Geometry => disjointGeoHelper(p.asInstanceOf[Disjoint[Geometry]])
      case v           => disjointHelper(p)
    }).asInstanceOf[Helper[Disjoint[T]]]

  def withinHelper[T](p: Within[T]) = new Helper[Within[T]](p) {
    val pvalue     = p.pvalue
    val pvalueList = equalsToList(pvalue)
    val comparable = (value: Any) =>
      value.isInstanceOf[Iterable[_]] || value.isInstanceOf[(_, _)] || value.isInstanceOf[(_, _, _)] || value
        .isInstanceOf[(_, _, _, _)] //nP contains comparable?
    val assert: Any => Boolean = {
      case v: ListSet[_]   => pvalueList.containsSlice(v.toList)
      case v: List[_]      => pvalueList.containsSlice(v)
      case v: Set[_]       => v.forall(pvalueList.contains)
      case v: Vector[_]    => pvalueList.containsSlice(v.toList)
      case v: (_, _)       => pvalueList.containsSlice(v.productIterator.toList)
      case v: (_, _, _)    => pvalueList.containsSlice(v.productIterator.toList)
      case v: (_, _, _, _) => pvalueList.containsSlice(v.productIterator.toList)
      case v               => false
    }
  }
  def withinStringHelper[T <: String](p: Within[T]) = new Helper[Within[T]](p) {
    val pvalue     = p.pvalue
    val comparable = (value: Any) => value.isInstanceOf[String] //???
    val assert: Any => Boolean = {
      case v: String => pvalue.contains(v)
    }
  }
  def withinGeoHelper[T <: Geometry](p: Within[T]) = new Helper[Within[T]](p) {
    val pvalue     = p.pvalue
    val comparable = (value: Any) => value.isInstanceOf[Geometry] //???
    val assert: Any => Boolean = {
      case v: Geometry => v.within(pvalue)
    }
  }
  override def within[T](p: Within[T]): Helper[Within[T]] =
    (p.pvalue match {
      case v: String   => withinStringHelper(p.asInstanceOf[Within[String]])
      case v: Geometry => withinGeoHelper(p.asInstanceOf[Within[Geometry]])
      case v           => withinHelper(p)
    }).asInstanceOf[Helper[Within[T]]]

  def startsWithHelper[T](p: Prefix[T]) = new Helper[Prefix[T]](p) {
    val pvalue     = p.pvalue
    val pvalueList = equalsToList(pvalue)
    val comparable = (value: Any) =>
      (value.isInstanceOf[Iterable[_]] && !value.isInstanceOf[Set[_]]) || value.isInstanceOf[(_, _)] || value
        .isInstanceOf[(_, _, _)] || value
        .isInstanceOf[(_, _, _, _)] //nP contains comparable?
    val assert: Any => Boolean = {
      case v: ListSet[_] => v.size >= pvalueList.size && v.zip(pvalueList).forall { case (v, p) => v == p }
      case v: List[_]    => v.size >= pvalueList.size && v.zip(pvalueList).forall { case (v, p) => v == p }
      case v: Set[_]     => v.size >= pvalueList.size && v.zip(pvalueList).forall { case (v, p) => v == p }
      case v: Vector[_]  => v.size >= pvalueList.size && v.zip(pvalueList).forall { case (v, p) => v == p }
      case v: (_, _) =>
        v.productIterator.toList.size >= pvalueList.size && v.productIterator.toList.zip(pvalueList).forall {
          case (v, p) => v == p
        }
      case v: (_, _, _) =>
        v.productIterator.toList.size >= pvalueList.size && v.productIterator.toList.zip(pvalueList).forall {
          case (v, p) => v == p
        }
      case v: (_, _, _, _) =>
        v.productIterator.toList.size >= pvalueList.size && v.productIterator.toList.zip(pvalueList).forall {
          case (v, p) => v == p
        }
    }
  }
  def startsWithStringHelper[T <: String](p: Prefix[T]) = new Helper[Prefix[T]](p) {
    val pvalue     = p.pvalue
    val comparable = (value: Any) => value.isInstanceOf[String] //???
    val assert: Any => Boolean = {
      case v: String => v.startsWith(pvalue)
    }
  }
  override def startsWith[T](p: Prefix[T]): Helper[Prefix[T]] =
    (p.pvalue match {
      case v: String => startsWithStringHelper(p.asInstanceOf[Prefix[String]])
      case v         => startsWithHelper(p)
    }).asInstanceOf[Helper[Prefix[T]]]

  def endsWithHelper[T](p: Suffix[T]) = new Helper[Suffix[T]](p) {
    val pvalue     = p.pvalue
    val pvalueList = equalsToList(pvalue)
    val comparable = (value: Any) =>
      (value.isInstanceOf[Iterable[_]] && !value.isInstanceOf[Set[_]]) || value.isInstanceOf[(_, _)] || value
        .isInstanceOf[(_, _, _)] || value
        .isInstanceOf[(_, _, _, _)] //nP contains comparable?
    val assert: Any => Boolean = {
      case v: ListSet[_] =>
        v.size >= pvalueList.size && v.toList.reverse.zip(pvalueList.reverse).forall { case (v, p) => v == p }
      case v: List[_] => v.size >= pvalueList.size && v.reverse.zip(pvalueList.reverse).forall { case (v, p) => v == p }
      case v: Set[_] =>
        v.size >= pvalueList.size && v.toList.reverse.zip(pvalueList.reverse).forall { case (v, p) => v == p }
      case v: Vector[_] =>
        v.size >= pvalueList.size && v.reverse.zip(pvalueList.reverse).forall { case (v, p) => v == p }
      case v: (_, _) =>
        v.productIterator.toList.size >= pvalueList.size && v.productIterator.toList.reverse
          .zip(pvalueList.reverse)
          .forall {
            case (v, p) => v == p
          }
      case v: (_, _, _) =>
        v.productIterator.toList.size >= pvalueList.size && v.productIterator.toList.reverse
          .zip(pvalueList.reverse)
          .forall {
            case (v, p) => v == p
          }
      case v: (_, _, _, _) =>
        v.productIterator.toList.size >= pvalueList.size && v.productIterator.toList.reverse
          .zip(pvalueList.reverse)
          .forall {
            case (v, p) => v == p
          }
    }
  }
  def endsWithStringHelper[T <: String](p: Suffix[T]) = new Helper[Suffix[T]](p) {
    val comparable = (value: Any) => value.isInstanceOf[String] //???
    val assert: Any => Boolean = {
      case v: String => v.endsWith(p.pvalue)
    }
  }
  override def endsWith[T](p: Suffix[T]): Helper[Suffix[T]] =
    (p.pvalue match {
      case v: String => endsWithStringHelper(p.asInstanceOf[Suffix[String]])
      case v         => endsWithHelper(p)
    }).asInstanceOf[Helper[Suffix[T]]]

  def regex(p: Regex): Helper[Regex] = new Helper[Regex](p) {
    val comparable = (value: Any) => value.isInstanceOf[String] //???
    val assert: Any => Boolean = {
      case v: String => p.pvalue.findFirstIn(v).isDefined
    }
  }

  def fuzzy[T](p: Fuzzy[T]): Helper[Fuzzy[T]] = p.pvalue match {
    case v: String => throw AssertionNotSupported("fuzzy string match not supported")
  }

  def and(p: And): Helper[And] = new Helper[And](p) {
    val ands                   = p.predicate.map(pToHelper)
    val comparable             = (value: Any) => ands.forall(_.comparable(value))
    val assert: Any => Boolean = (value: Any) => ands.forall(_.assert(value))
  }
  def or(p: Or): Helper[Or] = new Helper[Or](p) {
    val ors                    = p.predicate.map(pToHelper)
    val comparable             = (value: Any) => ors.exists(_.comparable(value))
    val assert: Any => Boolean = (value: Any) => ors.filter(_.comparable(value)).exists(_.assert(value))
  }

}
