package lspace.util

import java.nio.file.Paths

final case class Port(value: Int) extends AnyVal
case class ServicesConfig(port: Port = Port(80), graph: GraphConfig)

object ServicesConfig {
  import pureconfig._
  import pureconfig.generic.auto._

  implicit val memGraphReader: ConfigReader[GraphConfig] = ConfigReader
    .forProduct2("name", "path")(FileGraphConfig(_, _))
    .orElse(ConfigReader.forProduct1("name")(MemGraphConfig(_)))

  /**
    *
    * @param env variable name
    * @return
    */
  def config(env: String): ServicesConfig =
    pureconfig
      .loadConfig[ServicesConfig]
      .toOption
      .orElse(Option(System.getenv(env))
        .map { iri =>
          scribe.info(s"using $env=${iri}")
          pureconfig.loadConfig[ServicesConfig](Paths.get(iri)) match {
            case Right(r) => r
            case Left(e)  => throw new Exception(e.toString)
          }
        })
      .orElse(pureconfig
        .loadConfig[ServicesConfig](Paths.get("config/application.conf")) match {
        case Right(r) =>
          scribe.info("using local file 'config/application.conf'")
          Some(r)
        case Left(e) =>
          scribe.error(e.toString)
          None
      })
      .getOrElse {
        import com.typesafe.config.ConfigFactory
        scribe.warn("no context file found, starting in-memory graphs")
        pureconfig
          .loadConfig[ServicesConfig](ConfigFactory.parseString("""
                                                                  |{
                                                                  |  port : 80,
                                                                  |  graph : { name : "http://localhost" }
                                                                  |}
                                                                """.stripMargin))
          .getOrElse(throw new Exception("could not load any config ..."))
      }
}
