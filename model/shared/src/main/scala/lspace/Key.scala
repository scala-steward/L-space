package lspace

// implicit class WithKeyString[name <: String with Singleton](name: name) {
//   def k: Key[name]   = Key(name)
//   def key: Key[name] = Key(name)
// }
object Key:
  type Name = String & Singleton
  given fromString[name <: Name]: Conversion[name, Key[name]] = Key(_)

  // type ToKey[X] <: Key[X] = X match {
  //   case Name => Key[X]
  // }
  // def ToKey[X](x: X): ToKey[X] = x match {
  //   case s: String => Key(s).asInstanceOf[ToKey[X]]
  // }

  type KeyLike[X] <: Key[?] = X match {
    case Key[t] => Key[t]
    // case Name => ToKsey[X & Name]
  }
  def KeyLike[X](key: X): KeyLike[X] = (key match {
    case key: Key[?] => key
    // case _ => ToKey(key)
  }).asInstanceOf[KeyLike[X]]

case class Key[name <: Key.Name](name: name)

extension [name <: Key.Name](name: name) def key: Key[name] = Key(name)
