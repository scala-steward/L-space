package lspace.lgraph.provider.kafka

import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.structure.Node
import lspace.parse.JsonLD

case class LspaceKafkaProducer(topic: String) {
  import monix.kafka._
  import monix.execution.Scheduler
  import monix.reactive.Observable
  import org.apache.kafka.clients.producer.ProducerRecord

  implicit val scheduler: Scheduler = monix.execution.Scheduler.global

  private val producer: KafkaProducerSink[String, String] =
    KafkaProducerSink[String, String](producerCfg(), scheduler)

  private def producerCfg(hosts: List[String] = List()) =
    KafkaProducerConfig.default.copy(
      bootstrapServers = if (hosts.nonEmpty) hosts else List("localhost:9092")
    )

  def apply(nodes: Stream[Node]) =
    Observable
      .fromIterable(nodes)
      .map(JsonLD(MemGraphDefault).encode(_).toString())
      .map(new ProducerRecord[String, String](topic, _))
      .bufferIntrospective(1024)
      .consumeWith(producer)
}
