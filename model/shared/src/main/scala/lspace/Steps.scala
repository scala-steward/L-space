package lspace

// enum Step[T]:
//   case And()
//   case Has[T](t: T) extends Step[T]
sealed trait Step[+T]

object Has:
  def apply[p <: String](predicate: p): Has[predicate.type] = Has(predicate)
end Has
case class Has[P <: String] private (p: P) extends Step[Has[P]]

object In:
  def apply[p <: String](predicate: p): In[predicate.type] = In(predicate)

  type EndType[ET, X] = X match
    case _ => X
end In
case class In[predicate <: String] private (predicate: predicate) extends Step[predicate.type]

object Out:
  def apply[p <: String](predicate: p): Out[predicate.type] = Out(predicate)

  type EndType[ET, X] = X match
    case _ => X
end Out
case class Out[predicate <: String] private (predicate: predicate) extends Step[predicate.type]
