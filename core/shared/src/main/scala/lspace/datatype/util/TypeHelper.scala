package lspace.datatype.util

import java.time.{Instant, LocalDate, LocalTime}

import lspace.NS
import lspace.NS.types
import lspace.structure.IriResource
import lspace.types.geo.Geometry

import scala.annotation.tailrec

object TypeHelper {
  def isLiteral(value: Any): Boolean = value match {
    case _: IriResource => false
    case _: String      => true
    case _: Boolean     => true
    case _: Int         => true
    case _: Double      => true
    case _: Long        => true
    case _: Instant     => true
    case _: LocalDate   => true
    case _: LocalTime   => true
    case _: Geometry    => true
    case _ => throw new Exception(s"invalid type ${value.getClass.getSimpleName}")
  }
  def literalTypeIri(value: Any): Option[List[String]] =
    Option(value match {
      case _: IriResource => null
      case _: String      => NS.types.`@string` :: List()
      case _: Boolean     => NS.types.`@boolean` :: List()
      case _: Int         => NS.types.`@int` :: NS.types.`@double` :: NS.types.`@long` :: List()
      case _: Double =>
        NS.types.`@double` :: NS.types.`@int` :: NS.types.`@long` :: List()
      case _: Long => NS.types.`@long` :: NS.types.`@int` :: NS.types.`@double` :: List()
      case _: Instant =>
        NS.types.`@datetime` :: /*ldcontext.types.date :: ldcontext.types.time :: */ List()
      case _: LocalDate =>
        NS.types.`@date` :: /*ldcontext.types.datetime :: ldcontext.types.time :: */ List()
      case _: LocalTime =>
        NS.types.`@time` :: /*ldcontext.types.datetime :: ldcontext.types.date :: */ List()
      case _: Geometry => NS.types.`@geo` :: List()
      case _ => throw new Exception(s"invalid type ${value.getClass.getSimpleName}")
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
      case v => throw new Exception(s"unexpected type ${v.getClass.getSimpleName}")
    }
  }
}
