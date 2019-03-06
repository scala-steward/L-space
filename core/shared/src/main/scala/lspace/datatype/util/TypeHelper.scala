package lspace.datatype.util

import java.time.{Instant, LocalDate, LocalTime}

import lspace.NS
import lspace.NS.types
import lspace.structure.IriResource
import lspace.types.vector.Geometry

import scala.annotation.tailrec

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

  private val separators = Set('(', ')', '+')

  def getTypes(iri: String): (List[String], String) = {
    iri.splitAt(iri.indexWhere(separators.contains)) match {
      case ("", iri) if iri.startsWith(")") => List()    -> iri.drop(1)
      case ("", iri)                        => List(iri) -> ""
      case (iri, tail) if tail.startsWith("(") =>
        iri match {
          case types.`@list` =>
            val (valueTypes, newTail) = getTypes(tail.drop(1))
            valueTypes -> newTail
          case types.`@listset` =>
            val (valueTypes, newTail) = getTypes(tail.drop(1))
            valueTypes -> newTail
          case types.`@set` =>
            val (valueTypes, newTail) = getTypes(tail.drop(1))
            valueTypes -> newTail
          case types.`@vector` =>
            val (valueTypes, newTail) = getTypes(tail.drop(1))
            valueTypes -> newTail
          case types.`@map` =>
            val (keyTypes, newTail) = getTypes(tail.drop(1))
            if (!newTail.startsWith("(")) throw new Exception("map without second block")
            val (valueTypes, newTail2) = getTypes(newTail.drop(1))
            (keyTypes ++ valueTypes) -> newTail2
          case types.`@tuple` =>
            @tailrec
            def getT(tail: String, types: List[List[String]]): (List[List[String]], String) = {
              val (valueTypes, newTail) = getTypes(tail.drop(1))
              if (!newTail.startsWith("("))
                getT(newTail, types :+ (if (valueTypes.nonEmpty) valueTypes else List[String]()))
              else
                (types :+ (if (valueTypes.nonEmpty) valueTypes else List())) -> newTail
            }
            val (rangeTypes, newTail) = getT(tail, List())
            rangeTypes.flatten -> newTail
          case _ =>
            scribe.error("cannot parse : " + iri)
            throw new Exception("cannot parse : " + iri)
        }
      case (iri, tail) if tail.startsWith(")") => List(iri) -> tail.dropWhile(_ == ')')
      case (iri, tail) if tail.startsWith("+") =>
        val (tailTypes, newTail) = getTypes(tail.drop(1))
        (List(iri) ++ tailTypes) -> newTail
    }
  }
}
