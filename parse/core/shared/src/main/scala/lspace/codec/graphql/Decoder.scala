package lspace.codec.graphql

import lspace.codec.{ActiveContext, ActiveProperty}
import lspace.librarian.traversal.Step
import lspace.librarian.traversal.step.{HasLabel, In, Out}

//Work-In-Progress
trait Decoder {

  def toTraversal(graphql: String)(implicit activeContext: ActiveContext) = {
    graphql.filterNot(c => Set(' ', '\n').contains(c))
  }

  def toR(tail: String)(implicit activeContext: ActiveContext): List[Step] = tail.splitAt(1) match {
    case ("{", tail) => List()
    case _ =>
      tail.span(c => !Set('}', '{', ',').contains(c)) match {
        case (key, tail) if tail.startsWith("{") =>
          activePropertyToSteps(
            activeContext.definitions.all
              .getOrElse(key, throw new Exception("Active Property definition not found"))) ::: toR(tail)
        case (key, tail) if tail.startsWith("}") =>
          activePropertyToSteps(
            activeContext.definitions.all
              .getOrElse(key, throw new Exception("Active Property definition not found"))) ::: toR(tail)
//        case (key, tail) if tail.startsWith(",") => activePropertyToSteps(
        //            activeContext.definitions.all
        //              .get(key)
        //              .getOrElse(throw new Exception("Active Property definition not found"))) ::: toR(tail)
      }
  }

  def activePropertyToSteps(ap: ActiveProperty) =
    if (ap.`@reverse`) Out(Set(ap.property)) :: HasLabel(ap.`@type`) :: Nil
    else In(Set(ap.property)) :: HasLabel(ap.`@type`) :: Nil

}
