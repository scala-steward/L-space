package lspace

import java.util.concurrent.Executors
import monix.execution.ExecutionModel.AlwaysAsyncExecution
import monix.execution.{Scheduler, UncaughtExceptionReporter}

object ExecutionHelper {

  lazy val scheduledExecutor =
    Executors.newSingleThreadScheduledExecutor()

  // For actual execution of tasks
  lazy val executorService =
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
}
