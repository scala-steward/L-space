package lspace.lgraph.provider.kafka

import lspace.structure.Node

case class LspaceKafkaProducer(topic: String) {
  import monix.kafka._
  import monix.execution.Scheduler
  import monix.reactive.Observable
  import org.apache.kafka.clients.producer.ProducerRecord

  import lspace.Implicits.Scheduler.global

  private val producer: KafkaProducerSink[String, String] =
    KafkaProducerSink[String, String](producerCfg(), global)

  private def producerCfg(hosts: List[String] = List()) =
    KafkaProducerConfig.default.copy(
      bootstrapServers = if (hosts.nonEmpty) hosts else List("localhost:9092")
    )

  def apply(nodes: LazyList[Node]) =
    Observable
      .fromIterable(nodes)
      .map(lspace.codec.argonaut.Encoder(_).toString())
      .map(new ProducerRecord[String, String](topic, _))
      .bufferIntrospective(1024)
      .consumeWith(producer)
}
