package lspace

opaque type Name = String

object Name:
  def apply(value: String): Name = value

  extension (name: Name) def toString: String = name

end Name

opaque type Comment = String

object Comment:
  def apply(value: String): Comment = value

  extension (comment: Comment) def toString: String = comment

end Comment
