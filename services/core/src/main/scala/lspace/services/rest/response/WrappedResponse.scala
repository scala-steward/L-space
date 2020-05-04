package lspace.services.rest.response

trait WrappedResponse[T] {
  def response: T
}
