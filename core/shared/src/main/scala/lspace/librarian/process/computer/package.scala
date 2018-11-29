package lspace.librarian.process

import java.lang
import java.time.{Instant, LocalDate, LocalDateTime, LocalTime}

import lspace.librarian.process.traversal.Traverser
import lspace.librarian.structure.{Resource, Value}

package object computer {
  object task {
    def Mean(stream: Stream[Resource[_]]): Double = {
      val (sum, size) = stream.map(_.value).foldLeft[(Double, Int)](0.0, 0) {
        case ((sum: Double, size: Int), v: Int)    => (sum + v) -> (size + 1)
        case ((sum: Double, size: Int), v: Double) => (sum + v) -> (size + 1)
        case ((sum: Double, size: Int), v: Long)   => (sum + v) -> (size + 1)
        case ((sum: Double, size: Int), _)         => sum       -> size
      }
      //      if (valueStream.lengthCompare(0) == 0) lang.Double.NaN else valueStream.sum / valueStream.size
      if (size == 0) lang.Double.NaN else sum / size
    }

    def Sum[T](stream: Stream[Resource[T]]): T = {
      stream
        .map(_.value)
        .foldLeft[Any](0) {
          case (sum: Double, v: Int)    => sum + v
          case (sum: Double, v: Double) => sum + v
          case (sum: Double, v: Long)   => sum + v
          case (sum: Int, v: Int)       => sum + v
          case (sum: Int, v: Double)    => sum + v
          case (sum: Int, v: Long)      => sum + v
          case (sum: Long, v: Int)      => sum + v
          case (sum: Long, v: Double)   => sum + v
          case (sum: Long, v: Long)     => sum + v
          case (sum, v) =>
            throw new Exception(s"unexpected numeric type ${v.getClass}") //TODO: log warning and/or accept only a stream with numeric T's
        }
        .asInstanceOf[T]
    }
    //    stream.map(_.get.value).sum

    def Max[R <: Resource[_]](stream: Stream[Traverser[R]]): Stream[Traverser[R]] = {
      stream
        .reduceOption[Traverser[R]] {
          case (r1, r2) =>
            r1.get.value -> r2.get.value match {
              case (a: Int, b: Int)                     => if (a > b) r1 else r2
              case (a: Int, b: Double)                  => if (a > b) r1 else r2
              case (a: Int, b: Long)                    => if (a > b) r1 else r2
              case (a: Double, b: Int)                  => if (a > b) r1 else r2
              case (a: Double, b: Double)               => if (a > b) r1 else r2
              case (a: Double, b: Long)                 => if (a > b) r1 else r2
              case (a: Long, b: Int)                    => if (a > b) r1 else r2
              case (a: Long, b: Double)                 => if (a > b) r1 else r2
              case (a: Long, b: Long)                   => if (a > b) r1 else r2
              case (a: Instant, b: Instant)             => if (a.isAfter(b)) r1 else r2
              case (a: LocalDateTime, b: LocalDateTime) => if (a.isAfter(b)) r1 else r2
              case (a: LocalDate, b: LocalDate)         => if (a.isAfter(b)) r1 else r2
              case (a: LocalTime, b: LocalTime)         => if (a.isAfter(b)) r1 else r2
              case _ =>
                throw new Exception(s"unexpected numeric or temporal type ${r2.get.value.getClass}") //TODO: log warning and/or accept only a stream with numeric T's
            }
        }
        .toStream
      //      if (valueStream.lengthCompare(0) == 0) lang.Double.NaN else valueStream.max
    }

    def Min[R <: Resource[_]](stream: Stream[Traverser[R]]): Stream[Traverser[R]] = {
      stream
        .reduceOption[Traverser[R]] {
          case (r1, r2) =>
            r1.get.value -> r2.get.value match {
              case (a: Int, b: Int)                     => if (a < b) r1 else r2
              case (a: Int, b: Double)                  => if (a < b) r1 else r2
              case (a: Int, b: Long)                    => if (a < b) r1 else r2
              case (a: Double, b: Int)                  => if (a < b) r1 else r2
              case (a: Double, b: Double)               => if (a < b) r1 else r2
              case (a: Double, b: Long)                 => if (a < b) r1 else r2
              case (a: Long, b: Int)                    => if (a < b) r1 else r2
              case (a: Long, b: Double)                 => if (a < b) r1 else r2
              case (a: Long, b: Long)                   => if (a < b) r1 else r2
              case (a: Instant, b: Instant)             => if (a.isBefore(b)) r1 else r2
              case (a: LocalDateTime, b: LocalDateTime) => if (a.isBefore(b)) r1 else r2
              case (a: LocalDate, b: LocalDate)         => if (a.isBefore(b)) r1 else r2
              case (a: LocalTime, b: LocalTime)         => if (a.isBefore(b)) r1 else r2
              case _ =>
                throw new Exception(s"unexpected numeric or temporal type ${r2.get.value.getClass}") //TODO: log warning and/or accept only a stream with numeric T's
            }
        }
        .toStream
      //      if (valueStream.lengthCompare(0) == 0) lang.Double.NaN else valueStream.min
    }
  }
}
