package com.dewdrop.help

import com.dewdrop.help.view.{PowerView, TabsView, YourIdeasView}
import org.scalajs.dom

import scalatags.JsDom.all._

object HelpApp {
  private val VERSION = "0.9"
  private val view = div(
    `class` := "container-fluid",
    h1("пбл-утилиты", " ", small(VERSION)),
    TabsView(Map("Расклад" -> PowerView, "..." -> YourIdeasView)).view()
  )
  def main(args: Array[String]): Unit = {
    dom.document.body.appendChild(view.render)
  }
}
