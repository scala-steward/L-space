package lspace

import monix.execution.ExecutionModel.SynchronousExecution
import monix.execution.Scheduler

object ExecutionHelper {

  lazy val scheduler = Scheduler.computation(name = "lspace-librarian")

  lazy val syncscheduler = Scheduler.computation(name = "lspace-librarian-sync", executionModel = SynchronousExecution)
}
