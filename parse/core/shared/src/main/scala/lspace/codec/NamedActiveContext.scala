package lspace.codec

import scala.jdk.CollectionConverters._

class NamedActiveContext(val iri: String, private val activeContext: ActiveContext)
    extends ActiveContext(
      activeContext.`@prefix`(),
      activeContext.`@vocab`(),
      activeContext.`@language`(),
      activeContext.`@base`,
      activeContext.definitions(),
      activeContext.remotes
    ) {
  override def equals(o: Any): Boolean = o match {
    case activeContext: NamedActiveContext =>
      iri == activeContext.iri && this.activeContext.equals(activeContext.activeContext)
    case activeContext: ActiveContext => false
    case _                            => false
  }

  override def toString: String = s"NamedActiveContext:$iri"
}

object NamedActiveContext {
  def apply(iri: String, activeContext: ActiveContext): NamedActiveContext = new NamedActiveContext(iri, activeContext)

  private val cache: scala.collection.concurrent.Map[String, NamedActiveContext] =
    new java.util.concurrent.ConcurrentHashMap[String, NamedActiveContext](16, 0.9f, 32).asScala

  //TODO: create cache
  def get(iri: String): Option[NamedActiveContext]        = cache.get(iri)
  def cache(namedActiveContext: NamedActiveContext): Unit = cache.update(namedActiveContext.iri, namedActiveContext)
}
