//package lspace.services.play.controller
//
////import java.time.{ Duration, Instant, Period }
////import java.time.temporal.{ ChronoUnit, TemporalAmount }
//
//import akka.actor.{ Actor, ActorRef, ActorSystem, PoisonPill, Props }
//import akka.stream.Materializer
//import lspace.client.session.Session
//import lspace.client.io.{ ToServerProtocol, ToClientProtocol }
//import lspace.provider.mem.MemGraphDefault
//import lspace.structure.Node
//import lspace.services.play.security.{ ClientPlaySession, OpenPlaySession, UserPlaySession }
//import lspace.util.parse.LDParser
//import play.api.Logger
//import play.api.cache.SyncCacheApi
//import play.api.libs.json.{ JsArray, JsObject, JsValue }
//import play.api.libs.streams.ActorFlow
//import play.api.mvc.{ BaseController, ControllerComponents, WebSocket }
//import play.api.mvc.WebSocket.MessageFlowTransformer
//
//import scala.util.{ Failure, Success }
//
//trait JsonLDWebsocketController extends BaseController {
//  val log = Logger(this.getClass())
//
//  def components: ControllerComponents
//  implicit val system: ActorSystem
//  implicit val materializer: Materializer
//  def cache: SyncCacheApi
//  val parser: LDParser = MemGraphDefault.ldParser
//
//  implicit val messageFlowTransformer: MessageFlowTransformer[Any, Node] = {
//    MessageFlowTransformer.jsonMessageFlowTransformer.map(
//      json =>
//        json match {
//          case obj: JsObject =>
//            parser.fromJSON.resource(obj) match {
//              case Success(resource) => resource
//              case Failure(error) => throw error
//            }
//          case JsArray(values) => values.toList.map {
//            case obj: JsObject =>
//              parser.fromJSON.resource(obj) match {
//                case Success(resource) => resource
//                case Failure(error) => throw error
//              }
//            case _ =>
//          }
//          case _ =>
//        },
//      msg =>
//        parser.toJSON.nodeToJson(msg)._1.asInstanceOf[JsValue]
//    //        msg match {
//    //          case LDNotification.Traverse(traversal) => parser.toJSON.traversalToJson(traversal)
//    //          case LDNotification.Get(uri) => JsString(uri)
//    //        }
//    )
//  }
//
//  object DataServicesWebSocketActor {
//    def props(out: ActorRef) = Props(new DataServicesWebSocketActor(out))
//  }
//
//  class DataServicesWebSocketActor(out: ActorRef) extends Actor {
//    def receive = {
//      case ToServerProtocol.ToServerMessage.SetStatus(status) =>
//        val result = ToClientProtocol.ToClientMessage.NewData("test") //setStatus(status)
//        out ! result
//      //      case PairSession(sessionId, password) =>
//      //        cache.get[UserSession](sessionId) match {
//      //          case Some(userSession) => userSession.user match {
//      //            case Some(dataAccount) => {
//      //              if (password.isBcrypted(dataAccount.hashedPassword)) cache.set(sessionId, userSession.copy(out = Some(out)))
//      //            }
//      //            case None =>
//      //          }
//      //          case None =>
//      //        }
//      case m: ToClientProtocol.ToClientMessage.SessionProvisioned => out ! m //TODO: check if necessary
//    }
//
//    override def postStop() = {
//      self ! PoisonPill
//    }
//  }
//
//  def socket = WebSocket.accept[JsValue, JsValue] {
//    request =>
//      ActorFlow.actorRef(out => {
//        val socketActor = DataServicesWebSocketActor.props(out)
//        request.session.get("sId").flatMap(id => cache.get[Session](id)) match {
//          case Some(session) => {
//            session match {
//              case session: UserPlaySession => {
//                log.info("Pairing websocket channel to UserSession")
//                //                out ! SessionProvisioned(session.user.email) //TODO: replace with user-profile message
//                cache.set(session.iri, UserPlaySession(session.iri, Some(out), session.client, session.user, session.expiration))
//              }
//              case session: ClientPlaySession => {
//                log.info("Pairing websocket channel to ClientSession")
//                cache.set(session.iri, ClientPlaySession(session.iri, Some(out), session.client, session.expiration))
//              }
//              case session: OpenPlaySession => {
//                log.info("Pairing websocket channel to OpenSession")
//                cache.set(session.iri, OpenPlaySession(session.iri, Some(out), session.expiration))
//              }
//
//            }
//          }
//          case None => log.error("Could not pair websocket channel to session?!")
//        }
//        socketActor
//      })
//    //      request =>
//    //        Future.successful(request.session.get("user") match {
//    //          case None => Left(Forbidden)
//    //          case Some(_) => Right(ActorFlow.actorRef(out => DataServicesWebSocketActor.props(out)))
//    //        })
//  }
//}
