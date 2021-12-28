package lspace

// implicit class WithKeyString[name <: String with Singleton](name: name) {
//   def k: Key[name]   = Key(name)
//   def key: Key[name] = Key(name)
// }
object Key:
  type Name = String with Singleton
  given fromString[name <: String with Singleton]: Conversion[name, Key[name]] = Key(_)
// def apply[key <: Key]()
  type KeyLike[X] <: Key[?] = X match {
    case Key[t] => Key[t]
  }
  def KeyLike[X](key: X): KeyLike[X] = key match {
    case key: Key[?] => key // .asInstanceOf[X]
  }
// type Key = String with Singleton
case class Key[name <: String with Singleton](name: name)
