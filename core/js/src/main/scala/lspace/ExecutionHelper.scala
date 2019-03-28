package lspace

import monix.execution.ExecutionModel.AlwaysAsyncExecution
import monix.execution.Scheduler

object ExecutionHelper {
  lazy val scheduler =
//    Scheduler(executionModel = AlwaysAsyncExecution)
    Scheduler.trampoline(executionModel = AlwaysAsyncExecution)
//    Scheduler.global
}
