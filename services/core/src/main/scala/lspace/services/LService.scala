package lspace.services

object LService {}
trait LService[Req, Res, Service[_, _]] {
  def service: Service[Req, Res]
}
