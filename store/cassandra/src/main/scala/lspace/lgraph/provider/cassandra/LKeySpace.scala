package lspace.lgraph.provider.cassandra

case class LKeySpace(iri: String) {
  def graph: String      = "lspace-" + iri.takeRight(5) + iri.hashCode + "-graph"
  def data: String       = "lspace-" + iri.takeRight(5) + iri.hashCode + "-data"
  def ns: String         = "lspace-" + iri.takeRight(5) + iri.hashCode + "-ns"
  def nsIndex: String    = "lspace-" + iri.takeRight(5) + iri.hashCode + "-ns-index"
  def index: String      = "lspace-" + iri.takeRight(5) + iri.hashCode + "-index"
  def indexIndex: String = "lspace-" + iri.takeRight(5) + iri.hashCode + "-index-index"
}
