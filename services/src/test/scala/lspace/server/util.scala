package lspace.server

object util {
  import com.twitter.util.{Future => TwFuture}
  import scala.concurrent.{Future => ScFuture, Promise => ScPromise}
  implicit def twFutureToScala[T](twFuture: TwFuture[T]): ScFuture[T] = {
    val prom = ScPromise[T]()
    twFuture.onSuccess { res: T =>
      prom.success(res)
    }
    twFuture.onFailure { t: Throwable =>
      prom.failure(t)
    }
    prom.future
  }
}
