package com.dewdrop.help.view

import com.dewdrop.help.view.ElementOps._
import org.scalajs.dom.{Event, MouseEvent}

import scala.scalajs.js.timers._
import scala.util.Try
import scalatags.JsDom.all._

case class RangeControlView(label: String, from: Int, to: Int, initial: Int, changeTrigger: () => Unit) {
  private var _value: Int = initial
  def currentValue: Int = _value
  val rangeInput = input(`type` := "range", value := initial.toString, min := from.toString, max := to.toString).render
  val rangeLabel = div(span(s"$label: ($initial)")).render
  rangeInput.onmousemove = (e: MouseEvent) ⇒ {
    if (e.buttons == 1) {
      setTimeout(20) {
        handleNewValue(rangeInput.value)
      }
    }
  }
  rangeInput.onchange = (e: Event) ⇒ {
    handleNewValue(rangeInput.value)
    val maybeChanged = Try(rangeInput.value.toInt).toOption.map { i =>
      val changed = _value != i
      _value = i
      changed
    }
    if (maybeChanged.exists(identity)) {
      changeTrigger()
    }
  }
  private def handleNewValue(s: String): Unit = {
    Try(s.toInt).toOption.foreach { i ⇒
      rangeLabel.removeAllChildren()
      rangeLabel.appendChild(span(s"$label: ($i)").render)
    }
  }
}
