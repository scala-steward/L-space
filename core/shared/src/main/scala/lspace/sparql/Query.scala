package lspace.sparql

trait Query {}

object Variable {
  implicit def fromString(name: String): Variable = Variable(name)
}
case class Variable(name: String)
object Select {}
case class Select(variable: Variable*) extends Query {
  def WHERE(triple: (Variable, String, Variable)*): Query = ???
}
case class Ask()                                            extends Query
case class Construct(triple: (Variable, String, Variable)*) extends Query
case class Describe()                                       extends Query
