package lspace.util

object CacheStatus {
  case class CacheStatus(code: Int) extends AnyVal {
    override def toString: String = code.toString
  }
  val EMPTY   = CacheStatus(0) //Uncached status, links can be present
  val LOADING = CacheStatus(1)
  val CACHED  = CacheStatus(2)
  val FAILED  = CacheStatus(3)
}
