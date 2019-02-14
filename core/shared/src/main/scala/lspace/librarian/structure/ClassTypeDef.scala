package lspace.librarian.structure

object ClassTypeDef {}

trait ClassTypeDef[T <: ClassType[_]] {
  def classtype: T
}
