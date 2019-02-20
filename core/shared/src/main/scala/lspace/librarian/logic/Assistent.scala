package lspace.librarian.logic

import java.time.{Instant, LocalDate, LocalDateTime, LocalTime}

import lspace.librarian.logic.predicate._

trait Assistent {
  abstract class Helper[+T <: P[_]](val p: T) {
    def assert: Any => Boolean
    def comparable: Any => Boolean
  }
  trait OrderHelper {
    def int: (Int, Any) => Boolean
    def double: (Double, Any) => Boolean
    def long: (Long, Any) => Boolean
    def datetime: (Instant, Any) => Boolean
    def localdatetime: (LocalDateTime, Any) => Boolean
    def localdate: (LocalDate, Any) => Boolean
    def localtime: (LocalTime, Any) => Boolean
    def string: (String, Any) => Boolean
  }
  def orderhelper: OrderHelper
  def eqv[T](p: Eqv[T]): Helper[Eqv[T]]
  def neqv[T](p: Neqv[T]): Helper[Neqv[T]]
  def gt[T](p: Gt[T]): Helper[Gt[T]]
  def gte[T](p: Gte[T]): Helper[Gte[T]]
  def lt[T](p: Lt[T]): Helper[Lt[T]]
  def lte[T](p: Lte[T]): Helper[Lte[T]]
  def between[T](p: Between[T]): Helper[Between[T]]
  def outside[T](p: Outside[T]): Helper[Outside[T]]
  def inside[T](p: Inside[T]): Helper[Inside[T]]
  def intersect[T](p: Intersect[T]): Helper[Intersect[T]]
  def contains[T](p: Contains[T]): Helper[Contains[T]]
  def disjoint[T](p: Disjoint[T]): Helper[Disjoint[T]]
  def within[T](p: Within[T]): Helper[Within[T]]
  def startsWith[T](p: Prefix[T]): Helper[Prefix[T]]
  def endsWith[T](p: Suffix[T]): Helper[Suffix[T]]
  def regex(p: Regex): Helper[Regex]
  def fuzzy[T](p: Fuzzy[T]): Helper[Fuzzy[T]]
  def and(p: And): Helper[And]
  def or(p: Or): Helper[Or]

  implicit def pToHelper[T <: P[_]](p: T): Helper[T] =
    (p match {
      case p: Eqv[Any]       => eqv(p)
      case p: Neqv[Any]      => neqv(p)
      case p: Gt[Any]        => gt(p)
      case p: Gte[Any]       => gte(p)
      case p: Lt[Any]        => lt(p)
      case p: Lte[Any]       => lte(p)
      case p: Between[Any]   => between(p)
      case p: Inside[Any]    => inside(p)
      case p: Outside[Any]   => outside(p)
      case p: Intersect[Any] => intersect(p)
      case p: Contains[Any]  => contains(p)
      case p: Disjoint[Any]  => disjoint(p)
      case p: Within[Any]    => within(p)
      case p: Prefix[Any]    => startsWith(p)
      case p: Suffix[_]      => endsWith(p)
      case p: Regex          => regex(p)
      case p: Fuzzy[_]       => fuzzy(p)
      case p: And            => and(p)
      case p: Or             => or(p)
    }).asInstanceOf[Helper[T]]
}
