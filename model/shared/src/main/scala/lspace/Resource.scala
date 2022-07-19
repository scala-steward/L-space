package lspace

/** @tparam R
  *   resource self-type
  */
trait Resource[+R] extends Matchable derives CanEqual:
  /**
    * id is a unique identifier of a resource in a graph
    * @return
    */
  def id: Long

  /**
    * Get the graph that this resource is within.
    * @return
    */
  def graph: Graph

opaque type LResource[name, +R] <: Resource[R] = Resource[R]
