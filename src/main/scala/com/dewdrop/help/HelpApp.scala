package com.dewdrop.help

import com.dewdrop.help.view.{PowerView, TabsView, YourIdeasView}
import org.scalajs.dom

import scala.scalajs.js.JSApp
import scala.scalajs.js.annotation.JSExport
import scalatags.JsDom.all._

@JSExport
object HelpApp extends JSApp {
  private val VERSION = "0.4"
  private val view = div(`class` := "container-fluid",
    h1("пбл-утилиты", " ", small(VERSION)),
    TabsView(Map("Расклад" -> PowerView, "..." -> YourIdeasView)).view()
  )
  @JSExport
  override def main(): Unit = {
    dom.document.body.appendChild(view.render)
  }
}
