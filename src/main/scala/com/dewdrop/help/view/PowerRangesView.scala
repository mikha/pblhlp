package com.dewdrop.help.view

import org.scalajs.dom.Event
import org.scalajs.dom.html.{Element, Select}
import scalatags.JsDom
import scalatags.JsDom.all._

import scala.language.implicitConversions

case class PowerRangesView() extends View {
  private lazy val formInput =
    RangeControlView("Форма", 76, 110, 100)
  private lazy val fizaInput =
    RangeControlView("Физа", 75, 100, 100)
  private lazy val ageInput =
    RangeControlView("Возраст", 17, 35, 26)
  private lazy val _leaderLevelSelector = select(
    `class` := "form-control input-sm",
    width := 70,
    for (i <- 0 to 3) yield option(value := i, s"Л$i")
  ).render

  def form: Int = formInput.currentValue
  def fiza: Int = fizaInput.currentValue
  def age: Int = ageInput.currentValue
  def leaderLevel: Int = _leaderLevelSelector.value.toInt

  def setChangeTrigger(t: () => Unit): Unit = {
    formInput.setChangeTrigger(t)
    fizaInput.setChangeTrigger(t)
    ageInput.setChangeTrigger(t)
    _leaderLevelSelector.onchange = (_: Event) => t()
  }

  override def view(): JsDom.TypedTag[Element] =
    div(
      `class` := "form-inline",
      style := "padding: 5px",
      div(
        `class` := "form-group",
        style := "padding: 5px",
        width := "300px",
        formInput.rangeLabel,
        formInput.rangeInput
      ),
      div(
        `class` := "form-group",
        style := "padding: 5px",
        width := "300px",
        fizaInput.rangeLabel,
        fizaInput.rangeInput
      ),
      div(
        `class` := "form-group",
        style := "padding: 5px",
        width := "300px",
        ageInput.rangeLabel,
        ageInput.rangeInput
      )
    )

  def leaderLevelSelector: Select = _leaderLevelSelector
}
