package lspace

/** @tparam S
  *   outV-type, edge-start
  * @tparam E
  *   inV-type, edge-end
  */
trait Edge[+S, +E] extends Resource[Edge[S, E]]

case class OrphanEdge[name <: Key.Name, +S, +E](key: Key[name], from: Resource[S], to: Resource[E]) extends Edge[S, E]

opaque type LEdge[name, +S, +E] <: Edge[S, E] = Edge[S, E]
