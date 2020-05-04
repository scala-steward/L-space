package lspace.services.rest.request

trait WrappedRequest[T] {
  def request: T
}
