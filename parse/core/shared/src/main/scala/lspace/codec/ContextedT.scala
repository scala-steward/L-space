package lspace.codec

object ContextedT {
  implicit def withContext[T](t: T)(implicit activeContext: ActiveContext): ContextedT[T] = ContextedT(t)
}
case class ContextedT[+T](t: T)(implicit val activeContext: ActiveContext) {}
