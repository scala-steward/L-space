package lspace.sparql

//import lspace.types.string.Iri
//
//trait Query {}
//trait SelectQuery extends Query {
//  def DISTINCT(variable: Variable, variables: Variable*): Select = ???
//}
//trait ConstructQuery extends Query
//trait AskQuery       extends Query
//trait DescribeQuery  extends Query
//
//trait QueryForm
//
//object Variable {
//  implicit def fromString(name: String): Variable = Variable(name)
//}
//case class Variable(name: String)
//object Select {}
//case class Select(variable: Variable*) extends QueryForm {
//  def WHERE(triple: (Variable, String, Variable)*): Query = ???
//}
//
//case class Construct(triple: (Variable, String, Variable)*) extends QueryForm
//case class Construct(triple: (Variable, String, Variable)*) extends QueryForm
//
//object Query {
//  def SELECT(): SelectQuery                                    = ???
//  def SELECT(variable: Variable, variables: Variable*): Select = ???
//}
//
//case class Where()
//
//object A {
//  import Query._
//  SELECT DISTINCT ("abc", "def", "hij") WHERE ()
//}
