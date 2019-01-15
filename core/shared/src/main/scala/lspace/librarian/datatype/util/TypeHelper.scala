package lspace.librarian.datatype.util

import java.time.{Instant, LocalDate, LocalTime}

import lspace.NS
import lspace.librarian.structure.IriResource
import lspace.types.vector.Geometry

object TypeHelper {
  def isLiteral(value: Any): Boolean = value match {
    case v: IriResource => false
    case v: String      => true
    case v: Boolean     => true
    case v: Int         => true
    case v: Double      => true
    case v: Long        => true
    case v: Instant     => true
    case v: LocalDate   => true
    case v: LocalTime   => true
    case v: Geometry    => true
  }
  def literalTypeIri(value: Any): Option[List[String]] =
    Option(value match {
      case v: IriResource => null
      case v: String      => NS.types.`@string` :: List()
      case v: Boolean     => NS.types.`@boolean` :: List()
      case v: Int         => NS.types.`@int` :: NS.types.`@double` :: NS.types.`@long` :: List()
      case v: Double =>
        NS.types.`@double` :: NS.types.`@int` :: NS.types.`@long` :: List()
      case v: Long => NS.types.`@long` :: NS.types.`@int` :: NS.types.`@double` :: List()
      case v: Instant =>
        NS.types.`@datetime` :: /*ldcontext.types.date :: ldcontext.types.time :: */ List()
      case v: LocalDate =>
        NS.types.`@date` :: /*ldcontext.types.datetime :: ldcontext.types.time :: */ List()
      case v: LocalTime =>
        NS.types.`@time` :: /*ldcontext.types.datetime :: ldcontext.types.date :: */ List()
      case v: Geometry => NS.types.`@geo` :: List()
    })
}
