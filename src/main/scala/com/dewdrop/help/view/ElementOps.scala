package com.dewdrop.help.view

import org.scalajs.dom.html

import scala.language.implicitConversions

object ElementOps {
  implicit class CanBeHidden(e: html.Element) {
    def hide(): Unit = {
      e.setAttribute("hidden", "true")
      setClassNames(classNames() + "hidden")
    }
    def show(): Unit = {
      e.removeAttribute("hidden")
      setClassNames(classNames() - "hidden")
    }
    def isHidden: Boolean = Option(e.getAttribute("hidden")).isDefined
    def toggleHidden(): Unit = if (isHidden) show() else hide()
    def classNames(): Set[String] = Option(e.getAttribute("class")).getOrElse("").split(" ").toSet
    def setClassNames(names: Set[String]): Unit = e.setAttribute("class", names.mkString(" "))
    def addOrRemoveClass(condition: Boolean, `class`: String): Unit = {
      if (condition) setClassNames(classNames() + `class`) else setClassNames(classNames() - `class`)
    }
    def makeBold(bold: Boolean): Unit = e.style.fontWeight = if (bold) "bold" else "normal"
    def removeFirstChild(): Unit = Option(e.firstChild).foreach(c => e.removeChild(c))
    def removeAllChildren(): Unit = if (e.hasChildNodes()) {
      val childNodes = e.childNodes
      1 to childNodes.length map { i => childNodes(i - 1) } foreach { n => e.removeChild(n) }
    }
  }
}
