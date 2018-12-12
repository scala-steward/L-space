package lspace.lgraph.util

import java.time.Instant
import java.util.concurrent.TimeUnit

import lspace.lgraph.LGraph
import monix.execution.Scheduler.{global => scheduler}

import scala.concurrent.duration._

object CacheReaper {
  def apply(graph: LGraph): CacheReaper = new CacheReaper(graph)
}

/**
  * WIP: the CacheReaper must manage the amount of cache w.r.t. the maximum allowed memory-usage or store-config
  * @param graph
  */
class CacheReaper(graph: LGraph) {
  private[this] val reapingLock = new Object
  private val runtime           = Runtime.getRuntime
  private var reapAfter: Int    = 120
  val c = scheduler.scheduleWithFixedDelay(
    3,
    3,
    TimeUnit.SECONDS,
    new Runnable {
      def run(): Unit = {
        reapingLock.synchronized {
          if (runtime.freeMemory() / runtime.maxMemory().toDouble < 0.15) {
            println(s"free memory ${runtime.freeMemory()} :: ${runtime.maxMemory()} :: ${runtime.totalMemory()}")
//            def edgesCacheSize  = graph.edgeStore.totalCached()
//            def nodesCacheSize  = graph.edgeStore.totalCached()
//            def valuesCacheSize = graph.edgeStore.totalCached()
//
//            val totalCacheSizeBefore = edgesCacheSize + nodesCacheSize + valuesCacheSize

            val reaptime = Instant.now()

            val edgesToReap = graph.edgeStore
              .cached()
              .filter(_._lastused.plusSeconds(reapAfter).isBefore(reaptime))

            val nodesToReap = graph.nodeStore
              .cached()
              .filter(_._lastused.plusSeconds(reapAfter).isBefore(reaptime))

            val valuesToReap = graph.valueStore
              .cached()
              .filter(_._lastused.plusSeconds(reapAfter).isBefore(reaptime))

            graph.edgeStore.dropDeletedMarks(120)
            graph.nodeStore.dropDeletedMarks(120)
            graph.valueStore.dropDeletedMarks(120)

            nodesToReap.foreach(graph.nodeStore.uncacheByIri)
            valuesToReap.foreach(graph.valueStore.uncacheByIri)
            edgesToReap.foreach(graph.edgeStore.uncacheByIri)

            edgesToReap.foreach(graph.edgeStore.uncacheById)
            nodesToReap.foreach(graph.nodeStore.uncacheById)
            valuesToReap.foreach(graph.valueStore.uncacheById)

//            val totalCacheSizeAfter = edgesCacheSize + nodesCacheSize + valuesCacheSize
//
//            if (totalCacheSizeBefore / totalCacheSizeAfter < 0.1) {}

//            println(
//              s"reaper ${graph.iri} could uncache ${edgesToReap.size + nodesToReap.size + valuesToReap.size} resources")
            if (runtime.freeMemory() / runtime.maxMemory().toDouble < 0.20) reapAfter = 15.max(reapAfter - 15)
//            println(
//              s"reaping ${graph.iri} cache took ${Instant.now().toEpochMilli - reaptime.toEpochMilli} milli-seconds")
          } else {
            if (runtime.freeMemory() / runtime.maxMemory().toDouble > 0.40) reapAfter = 240.min(reapAfter + 15)
          }
        }
      }
    }
  )

  def kill(): Unit = c.cancel()
}
