package com.dewdrop.help.view

import org.scalajs.dom.{DragEvent, Event, MouseEvent}
import org.scalajs.dom.html.{Element, Heading}
import ElementOps._

import scala.scalajs.js.timers._
import scala.util.Try
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all._

object PowerView extends View {
  private val powerThresholdInput = input(`type` := "text", `class` := "form-control", placeholder := "вставляйте цифры сюда").render
  powerThresholdInput.onpaste = (e: DragEvent) => {
    setTimeout(200) {
      handleThresholdInput(powerThresholdInput.value)
    }
  }
  private val powerOutput = div().render
  override def view(): TypedTag[Element] = {
    div(
      h4("расчет расклада силы игрока по границам уровня"),
      p("обладатели платного пакета и 4-го уровня скаута видят следующие данные для любого игрока:"),
      img(src := "scout-exp-power.png", `class` := "img-responsive"),
      p(style := "margin-top: 10px;",
        "выделяйте цифры для ", strong("нижней границы уровня"),
        " и копируйте в поле ниже",
        i(" (одно число считается как вратарь)"), ":"),
      div(`class` := "input-group",
        span(`class` := "input-group-btn",
          button(`type` := "button", `class` := "btn btn-primary", onclick := clearAll _, "Очистить")
        ),
        powerThresholdInput
      ),
      powerOutput
    )
  }
  private def clearAll(e: Event): Unit = {
    powerThresholdInput.value = ""
    handleThresholdInput("")
    powerThresholdInput.focus()
  }
  private def handleThresholdInput(in: String): Unit = {
    val powerView = in match {
      case OutfieldExp(defExp, midExp, attExp) =>
        val defPower = level(defExp)
        val midPower = level(midExp)
        val attPower = level(attExp)
        Some(OutfieldPositionalPowerView(defPower, midPower, attPower))
      case KeeperExp(gkExp) =>
        val gkPower = level(gkExp) + 30
        Some(KeeperPositionalPowerView(gkPower))
      case _ => None
    }
    powerOutput.removeAllChildren()
    powerView.foreach { view =>
      powerOutput.appendChild(view.header)
      powerOutput.appendChild(view.view().render)
    }
  }
  private val levels = 1 to 75 map { i => (10700 * math.pow(1.0427, i) - 10698).toInt }
  def level(exp: Int): Int = {
    levels.indexWhere(_ > exp)
  }
}

object OutfieldExp {
  def unapply(arg: String): Option[(Int, Int, Int)] = {
    arg.replace(" ", "").trim.split("\t") match {
      case Array(one, two, three) => Try((one.toInt, two.toInt, three.toInt)).toOption
      case _ => None
    }
  }
}
object KeeperExp {
  def unapply(arg: String): Option[Int] = {
    arg.replace(" ", "").trim.split("\t") match {
      case Array(one) => Try(one.toInt).toOption
      case _ => None
    }
  }
}

trait PositionalPowerView extends View {
  private lazy val formInput = RangeControlView("Форма", 76, 110, 100, calcAndRenderPowers)
  private lazy val fizaInput = RangeControlView("Физа", 75, 100, 100, calcAndRenderPowers)
  private lazy val ageInput = RangeControlView("Возраст", 17, 35, 26, calcAndRenderPowers)
  import PositionalPower._
  protected def posPowerWithFormAndFiza(posPower: => Double): Double = {
    withFormFiza(posPower, formInput.currentValue, fizaInput.currentValue, ageInput.currentValue)
  }
  protected def allPosPowers: Seq[(String, Double)]
  def header: Heading
  private lazy val posPowersOutput = div(`class` := "list-group").render
  private def calcAndRenderPowers(): Unit = {
    val newPosPowers = allPosPowers
    val maxPosPower = newPosPowers.map(_._2).max
    val newPosPowerChildren = newPosPowers.map {
      case (position, power) =>
        div(`class` := s"list-group-item${if (power == maxPosPower) " list-group-item-info" else ""}",
          div(`class` := "row",
            div(`class` := "col-md-1 col-xs-2", if (power == maxPosPower) strong(position) else position),
            div(`class` := "col-md-2 col-xs-3", if (power == maxPosPower) strong(power) else power)
          )
        ).render
    }
    posPowersOutput.removeAllChildren()
    newPosPowerChildren.foreach(posPowersOutput.appendChild)
  }
  calcAndRenderPowers()
  override def view(): TypedTag[Element] = {
    div(
      div(`class` := "form-inline", style := "padding: 5px",
        div(`class` := "form-group", style := "padding: 5px", width := "300px",
          formInput.rangeLabel, formInput.rangeInput
        ),
        div(`class` := "form-group", style := "padding: 5px", width := "300px",
          fizaInput.rangeLabel, fizaInput.rangeInput
        ),
        div(`class` := "form-group", style := "padding: 5px", width := "300px",
          ageInput.rangeLabel, ageInput.rangeInput
        )
      ),
      posPowersOutput
    )
  }
}
case class OutfieldPositionalPowerView(defPower: Int, midPower: Int, attPower: Int) extends PositionalPowerView {
  private lazy val sum = defPower + midPower + attPower
  lazy val header: Heading = h3(defPower, " + ", midPower, " + ", attPower, " = ", defPower + midPower + attPower).render
  import PositionalPower._
  override protected def allPosPowers: Seq[(String, Double)] = Seq(
    "xD" -> posPowerWithFormAndFiza(XD(defPower, midPower, attPower, sum)),
    "xB" -> posPowerWithFormAndFiza(XB(defPower, midPower, attPower, sum)),
    "DM" -> posPowerWithFormAndFiza(DM(defPower, midPower, attPower, sum)),
    "xM" -> posPowerWithFormAndFiza(XM(defPower, midPower, attPower, sum)),
    "AM" -> posPowerWithFormAndFiza(AM(defPower, midPower, attPower, sum)),
    "xW" -> posPowerWithFormAndFiza(XW(defPower, midPower, attPower, sum)),
    "xF" -> posPowerWithFormAndFiza(XF(defPower, midPower, attPower, sum)),
    "ST" -> posPowerWithFormAndFiza(ST(defPower, midPower, attPower, sum))
  )
}
case class KeeperPositionalPowerView(gkPower: Int) extends PositionalPowerView {
  lazy val header: Heading = h3(gkPower).render
  import PositionalPower._
  override protected def allPosPowers: Seq[(String, Double)] = Seq(
    "GK" -> posPowerWithFormAndFiza(GK(gkPower))
  )
}

case class RangeControlView(label: String, from: Int, to: Int, initial: Int, changeTrigger: () => Unit) {
  private var _value: Int = initial
  def currentValue: Int = _value
  val rangeInput = input(`type` := "range", value := initial.toString, min := from.toString, max := to.toString).render
  val rangeLabel = div(span(s"$label: ($initial)")).render
  rangeInput.onmousemove = (e: MouseEvent) => {
    if (e.buttons == 1) {
      setTimeout(20) {
        handleNewValue(rangeInput.value)
      }
    }
  }
  rangeInput.onchange = (e: Event) => {
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
    Try(s.toInt).toOption.foreach { i =>
      rangeLabel.removeAllChildren()
      rangeLabel.appendChild(span(s"$label: ($i)").render)
    }
  }
}

object PositionalPower {
  def roundUp(d: Double): Double = BigDecimal(d).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
  def GK(g: Int): Double = roundUp(if (g > 30) Math.pow(1.0178, g - 30.0) * 35.0 else g.toDouble)
  def XD(d: Int, m: Int, a: Int, sum: Int): Double = roundUp((math.min(d, sum * 0.80) + math.min(m, sum * 0.20)) * 1.4)
  def DM(d: Int, m: Int, a: Int, sum: Int): Double = roundUp((math.min(d, sum * 0.40) + math.min(m, sum * 0.45) + math.min(a, sum * 0.15)) * 1.15)
  def XB(d: Int, m: Int, a: Int, sum: Int): Double = roundUp((math.min(d, sum * 0.45) + math.min(m, sum * 0.40) + math.min(a, sum * 0.15)) * 1.15)
  def XM(d: Int, m: Int, a: Int, sum: Int): Double = roundUp((math.min(d, sum * 0.15) + math.min(m, sum * 0.70) + math.min(a, sum * 0.15)) * 1.3)
  def AM(d: Int, m: Int, a: Int, sum: Int): Double = roundUp((math.min(d, sum * 0.10) + math.min(m, sum * 0.50) + math.min(a, sum * 0.40)) * 1.18)
  def XW(d: Int, m: Int, a: Int, sum: Int): Double = roundUp((math.min(d, sum * 0.10) + math.min(m, sum * 0.45) + math.min(a, sum * 0.45)) * 1.15)
  def XF(d: Int, m: Int, a: Int, sum: Int): Double = roundUp((math.min(m, sum * 0.20) + math.min(a, sum * 0.80)) * 1.22)
  def ST(d: Int, m: Int, a: Int, sum: Int): Double = roundUp(math.min(a, sum * 1.00) * 1.6)
  def withFormFiza(base: Double, form: Int, fiza: Int, age: Int): Double = {
    val x = 0.284 * age * age - 8.47 * age + 49
    val formNorm = Math.max(83, if (form < 100) form.toDouble * (100 - x) / 100 + x else form.toDouble)
    val fizaKoef = formNorm * fiza / 100
    roundUp((base - (100 - fizaKoef) / 2.5) * (fizaKoef / 50 - (fizaKoef * fizaKoef) / 10000))
  }
}