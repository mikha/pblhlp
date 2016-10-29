package com.dewdrop.help.view
import org.scalajs.dom.html.Element
import ElementOps._
import org.scalajs.dom.raw.MouseEvent

import scalatags.JsDom.TypedTag
import scalatags.JsDom.all._

case class TabsView(tabs: Map[String, View]) extends View {
  private val content = div().render
  private val tabElements = tabs.keys.map(tabName => {
    val element = li(role := "presentation", a(href := "#", tabName)).render
    element.onclick = (e: MouseEvent) => {
      setActive(tabName)
    }
    tabName -> element
  }).toMap
  override def view(): TypedTag[Element] = {
    setActive(tabs.keys.head)
    div(
      ul(`class` := "nav nav-tabs", tabElements.values.toSeq),
      content
    )
  }
  private def setActive(tabName: String): Unit = {
    tabElements.foreach {
      case (n, e) => e.addOrRemoveClass(n == tabName, "active")
    }
    content.removeFirstChild()
    tabs.get(tabName).foreach(c => content.appendChild(c.view().render))
  }
}
