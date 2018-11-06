package lspace.client.session

import scala.scalajs.js
import scala.scalajs.js.JSON

/**
  * Created by thijs on 20-9-16.
  */
object AppServices {
  import org.scalajs.dom

  import scala.concurrent.Future
  import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

  val domain = s"${dom.window.location.hostname}:9002"
  //  val domain = "data.int.l-space.eu"
  val baseUri = s"${dom.window.location.protocol}//$domain"
  //  //    val baseUri = s"${dom.window.location.protocol}//data.int.l-space.eu"
  //  def removeBaseUri(uri: String) = uri.replaceFirst("data.l-space.eu", "")
  //  def getUri(uri: String) = baseUri + removeBaseUri(uri)

  /**
    * restore session if any
    * @return
    */
  def session: Future[(Option[String], Option[String])] = {
    dom.ext.Ajax.get(url = baseUri + "/session", withCredentials = true).map { response =>
      val json = JSON.parse(response.responseText).asInstanceOf[js.Dictionary[js.Any]]
      (json.get("session").asInstanceOf[Option[String]], json.get("email").asInstanceOf[Option[String]])
    }
  }

  def login(loginName: String, password: String) = {

    //    getSalt(loginName).flatMap { salt =>
    //      dom.ext.Ajax.post(
    //        url = baseUri + "/account/" + loginName + "/login",
    //        data = BCrypt.hashSync(password, salt),
    //        withCredentials = true
    //      )
    //    }
    dom.ext.Ajax
      .post(url = baseUri + "/account/" + loginName + "/login", data = password, withCredentials = true)
      .map { result =>
        //        BaseApp.newEvent(Login(loginName))
        result
      }
  }

  def getProfile(loginName: String) = {
    dom.ext.Ajax.get(url = baseUri + "/account/" + loginName + "/profile", withCredentials = true)
  }

  def logout: Future[Option[String]] = {
    dom.ext.Ajax.get(url = baseUri + "/logout", withCredentials = true).map { response =>
      //      BaseApp.newEvent(Logout)
      val json = JSON.parse(response.responseText).asInstanceOf[js.Dictionary[js.Any]]
      json.get("session").asInstanceOf[Option[String]]
    }
  }

}
