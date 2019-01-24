package lspace.parse

import lspace.librarian.structure.{ClassType, Node, Property}

import scala.collection.immutable.{HashSet, ListMap}

/**
  *
  * @param context base-iri -> prefix
  * @param language
  */
case class LDContextBuilder(context: ListMap[String, String] = ListMap[String, String](),
                            typeMods: ListMap[Property, ClassType[_]] = ListMap[Property, ClassType[_]](),
                            language: String = "en") {

  /**
    *
    * @param key
    * @param languageOption defaults to "en" which is default when no language is set
    * @return
    */
  def compactIri(key: ClassType[_], languageOption: Option[String] = None): (String, LDContextBuilder) = {
    key.label.get(languageOption.getOrElse(language)) match {
      case Some(label) if label.nonEmpty =>
        val uriBase = key.iri.stripSuffix(label)
        if (uriBase != key.iri) {
          context.get(uriBase).map(_ + ":" + label).map(iri => iri -> this).getOrElse {
            val prefix = context.size.toString
            s"$prefix:$label" -> this.copy(context = context + (uriBase -> prefix))
          }
        } else key.iri -> this
      case _ =>
        key.iri -> this
    }
  }
}
