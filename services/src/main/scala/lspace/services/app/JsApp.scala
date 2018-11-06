package lspace.services.app

import scalajs.html.scripts
import scalatags.Text
import scalatags.Text.all
import scalatags.Text.all._
import scalatags.Text.tags2.title

case class JsApp(id: String, name: String, resources: Seq[Text.TypedTag[String]]) {

  val rendered: String =
    "<!DOCTYPE html>" + html(
      head(
        Seq(
          meta(all.name := "viewport", content := "initial-scale=1.0", charset := "UTF-8"),
          title(name),
          link(rel := "shortcut icon", `type` := "image/png", href := "/assets/images/favicon.png")
        ) ++ resources),
      body(raw(scripts(projectName = id,
                       name => s"/assets/$name",
                       name => getClass.getResource(s"/public/$name") != null).body))
    ).render
}
