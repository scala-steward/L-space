package lspace.util

import org.slf4j.LoggerFactory

import scala.util.control.NonFatal

/**
  * Created by thijs on 20-8-16.
  */
object Retry {
  val log = LoggerFactory.getLogger(getClass)

  //  def retry[T](n: Int)(f: => Future[T])(implicit ec: ExecutionContext): Future[T] = {
  //    f map (o => o) recoverWith {
  //      case e if n > 1 => {
  //        log.debug("retrying " + n + " more times after error: " + e.getMessage)
  //        Thread.sleep(1000)
  //        retry(n - 1)(f)
  //      }
  //      case e => Future.failed(e)
  //    } map (o => o)
  //  }

  /**
    * Right Away
    * @param maxTries maximum number of retries
    * @param tries
    * @param f function to retry
    * @tparam T
    * @return
    */
  def ra[T](maxTries: Int, tries: Int = 0)(f: => T)(recover: => Unit): T = {
    try {
      f
    } catch {
      case NonFatal(e) =>
        recover
        if (tries < maxTries) {
          log.debug("retrying " + (maxTries - tries) + " more times after error: " + e.getMessage)
          ra(maxTries, tries + 1)(f)(recover)
        } else throw e
    }
  }

  /**
    * Delayed
    * @param maxTries maximum number of retries
    * @param delay time before retrying
    * @param tries
    * @param f function to retry
    * @tparam T
    * @return
    */
  def delayed[T](maxTries: Int, delay: Long, tries: Int = 0)(f: => T)(recover: => Unit): T = {
    try {
      f
    } catch {
      case NonFatal(e) =>
        recover
        if (tries < maxTries) {
          val waitTime = delay
          log.debug("retrying " + (maxTries - tries) + s" more times in $waitTime ms after error: " + e.getMessage)
          Thread.sleep(delay)
          delayed(maxTries, delay, tries + 1)(f)(recover)
        } else throw e
    }
  }

  /**
    * Exponential
    * @param maxTries
    * @param tries
    * @param f
    * @param recover
    * @tparam T
    * @return
    */
  def exp[T](maxTries: Int, tries: Int = 0)(f: => T)(recover: => Unit): T = {
    try {
      f
    } catch {
      case NonFatal(e) =>
        recover
        if (tries < maxTries) {
          val waitTime = Math.pow(2, tries).toLong
          log.debug("retrying " + (maxTries - tries) + s" more times in $waitTime ms after error: " + e.getMessage)
          Thread.sleep(waitTime)
          exp(maxTries, tries + 1)(f)(recover)
        } else throw e
    }
  }

  /**
    * Exponential with random
    * @param maxTries
    * @param tries
    * @param f
    * @param recover
    * @tparam T
    * @return
    */
  def varExp[T](maxTries: Int, tries: Int = 0)(f: => T)(recover: => Unit): T = {
    try {
      f
    } catch {
      case NonFatal(e) =>
        recover
        if (tries < maxTries) {
          val expTime  = Math.pow(2, tries)
          val waitTime = (expTime * Math.random() + expTime / 2).toLong
          log.debug("retrying " + (maxTries - tries) + s" more times in $waitTime ms after error: " + e.getMessage)
          Thread.sleep(waitTime)
          varExp(maxTries, tries + 1)(f)(recover)
        } else throw e
    }
  }
}
