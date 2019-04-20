package lspace.codec

case class ContextedT[T](t: T)(implicit val activeContext: ActiveContext) {}
