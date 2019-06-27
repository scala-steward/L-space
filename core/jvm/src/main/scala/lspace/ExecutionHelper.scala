package lspace

import java.util.concurrent.Executors

import monix.execution.ExecutionModel.{AlwaysAsyncExecution, SynchronousExecution}
import monix.execution.{Scheduler, UncaughtExceptionReporter}

object ExecutionHelper {

  lazy val scheduledExecutor =
//    Executors.newSingleThreadExecutor()
    Executors.newSingleThreadScheduledExecutor()
//    Executors.newScheduledThreadPool(8)

  // For actual execution of tasks
  lazy val executorService =
//    Executors.newFixedThreadPool(8)
//    scala.concurrent.ExecutionContext.fromExecutor(scheduledExecutor)
    scala.concurrent.ExecutionContext.Implicits.global

  // Logs errors to stderr or something
  lazy val uncaughtExceptionReporter =
    UncaughtExceptionReporter(executorService.reportFailure)

  lazy val scheduler = Scheduler(
    scheduledExecutor,
    executorService,
    uncaughtExceptionReporter,
    AlwaysAsyncExecution
  )
  lazy val syncscheduler = Scheduler(
    scheduledExecutor,
    executorService,
    uncaughtExceptionReporter,
    SynchronousExecution
  )
}
