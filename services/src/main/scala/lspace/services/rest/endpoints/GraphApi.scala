package lspace.services.rest.endpoints

trait GraphApi {
  def nodes: NodesApi
  def edges: EdgesApi
  def values: ValuesApi
  def resources: ResourcesApi
  def g: ExecutionApi
}
